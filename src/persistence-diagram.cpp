#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <stdexcept>

#include "ttk_compat.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List persistence_diagram_cpp(const Rcpp::List& mesh,
                                   const std::string& scalar_field_name,
                                   double persistence_threshold) {
  try {
    // 1. Extract mesh data (reuse the same structure as critical points)
    if (!mesh.containsElementNamed("vb")) {
      throw std::runtime_error("mesh3d must contain 'vb' (vertex matrix)");
    }
    
    Rcpp::NumericMatrix vb = Rcpp::as<Rcpp::NumericMatrix>(mesh["vb"]);
    if (vb.nrow() < 3) {
      throw std::runtime_error("Vertex matrix must have at least 3 rows");
    }
    
    ttk::SimplexId vertex_count = vb.ncol();
    std::vector<float> vertices(vertex_count * 3);
    
    for (ttk::SimplexId i = 0; i < vertex_count; i++) {
      vertices[i * 3]     = static_cast<float>(vb(0, i));
      vertices[i * 3 + 1] = static_cast<float>(vb(1, i));
      vertices[i * 3 + 2] = static_cast<float>(vb(2, i));
    }
    
    // Extract triangles
    if (!mesh.containsElementNamed("it")) {
      throw std::runtime_error("mesh3d must contain 'it' (triangle indices)");
    }
    
    Rcpp::IntegerMatrix it = Rcpp::as<Rcpp::IntegerMatrix>(mesh["it"]);
    ttk::SimplexId triangle_count = it.ncol();
    
    std::vector<ttk::LongSimplexId> connectivity(triangle_count * 3);
    std::vector<ttk::LongSimplexId> offsets(triangle_count + 1);
    
    for (ttk::SimplexId i = 0; i < triangle_count; i++) {
      offsets[i] = i * 3;
      connectivity[i * 3]     = static_cast<ttk::LongSimplexId>(it(0, i)) - 1;
      connectivity[i * 3 + 1] = static_cast<ttk::LongSimplexId>(it(1, i)) - 1;
      connectivity[i * 3 + 2] = static_cast<ttk::LongSimplexId>(it(2, i)) - 1;
    }
    offsets[triangle_count] = triangle_count * 3;
    
    // Extract scalar field
    if (!mesh.containsElementNamed(scalar_field_name.c_str())) {
      throw std::runtime_error("Mesh does not contain scalar field: " + scalar_field_name);
    }
    
    Rcpp::NumericVector scalar_r = Rcpp::as<Rcpp::NumericVector>(mesh[scalar_field_name.c_str()]);
    if (scalar_r.size() != vertex_count) {
      throw std::runtime_error("Scalar field length does not match vertex count");
    }
    
    std::vector<float> scalar_field(vertex_count);
    for (ttk::SimplexId i = 0; i < vertex_count; i++) {
      scalar_field[i] = static_cast<float>(scalar_r[i]);
    }
    
    // 2. Create TTK triangulation
    ttk::Triangulation triangulation;
    triangulation.setInputPoints(vertex_count, vertices.data());
    
    ttk_compat::setInputCells(triangulation, triangle_count, connectivity, offsets);
    ttk_compat::preconditionTriangulation(triangulation);
    
    // 3. Create order array
    std::vector<ttk::SimplexId> order(vertex_count);
    ttk_compat::preconditionOrderArray(vertex_count,
                                       scalar_field.data(),
                                       order.data());
    
    // 4. Compute persistence diagram
    ttk::PersistenceDiagram diagram;
    std::vector<ttk::PersistencePair> diagram_output;
    
    int result = ttk_compat::executePersistenceDiagram(
      diagram,
      diagram_output,
      scalar_field.data(),
      order.data(),
      &triangulation
    );
    
    if (result != 0) {
      throw std::runtime_error("TTK persistence diagram computation failed");
    }
    
    // 5. Process results
    // PersistencePair contains birth and death vertices
    std::vector<double> birth_values;
    std::vector<double> death_values;
    std::vector<int> birth_ids;
    std::vector<int> death_ids;
    std::vector<int> dimensions;
    std::vector<double> persistences;
    
    for (size_t i = 0; i < diagram_output.size(); i++) {
      const auto& pair = diagram_output[i];
      
      ttk::SimplexId birth_id = pair.birth.id;
      ttk::SimplexId death_id = pair.death.id;
      
      double birth_val = scalar_field[birth_id];
      double death_val = scalar_field[death_id];
      double persistence = std::abs(death_val - birth_val);
      
      // Apply threshold filter
      if (persistence >= persistence_threshold) {
        birth_ids.push_back(birth_id + 1);
        death_ids.push_back(death_id + 1);
        birth_values.push_back(birth_val);
        death_values.push_back(death_val);
        dimensions.push_back(pair.dim);
        persistences.push_back(persistence);
      }
    }
    
    // 6. Build result data frame
    int n_pairs = birth_ids.size();
    
    return Rcpp::List::create(
      Rcpp::Named("birth_id") = Rcpp::wrap(birth_ids),
      Rcpp::Named("death_id") = Rcpp::wrap(death_ids),
      Rcpp::Named("birth_value") = Rcpp::wrap(birth_values),
      Rcpp::Named("death_value") = Rcpp::wrap(death_values),
      Rcpp::Named("dimension") = Rcpp::wrap(dimensions),
      Rcpp::Named("persistence") = Rcpp::wrap(persistences),
      Rcpp::Named("n_pairs") = n_pairs,
      Rcpp::Named("scalar_field") = scalar_field_name,
      Rcpp::Named("total_vertices") = vertex_count,
      Rcpp::Named("total_triangles") = triangle_count
    );
    
  } catch (const std::exception& e) {
    Rcpp::stop(std::string("Error in persistence_diagram: ") + e.what());
  } catch (...) {
    Rcpp::stop("Unknown error in persistence_diagram");
  }
  
  return Rcpp::List();
}