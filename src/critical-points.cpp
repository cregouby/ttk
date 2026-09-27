#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <stdexcept>

// Use the compatibility layer instead of direct TTK includes
#include "ttk_compat.h"

using namespace Rcpp;

struct MeshData {
  std::vector<float> vertices;
  std::vector<ttk::LongSimplexId> connectivity;
  std::vector<ttk::LongSimplexId> offsets;
  std::vector<float> scalar_field;
  ttk::SimplexId vertex_count;
  ttk::SimplexId triangle_count;
};

MeshData extract_mesh3d_data(const Rcpp::List& mesh,
                             const std::string& scalar_field_name) {
  MeshData data;
  
  if (!mesh.containsElementNamed("vb")) {
    throw std::runtime_error("mesh3d must contain 'vb' (vertex matrix)");
  }
  
  Rcpp::NumericMatrix vb = Rcpp::as<Rcpp::NumericMatrix>(mesh["vb"]);
  if (vb.nrow() < 3) {
    throw std::runtime_error("Vertex matrix must have at least 3 rows");
  }
  
  data.vertex_count = vb.ncol();
  data.vertices.resize(data.vertex_count * 3);
  
  for (ttk::SimplexId i = 0; i < data.vertex_count; i++) {
    data.vertices[i * 3]     = static_cast<float>(vb(0, i));
    data.vertices[i * 3 + 1] = static_cast<float>(vb(1, i));
    data.vertices[i * 3 + 2] = static_cast<float>(vb(2, i));
  }
  
  if (!mesh.containsElementNamed("it")) {
    throw std::runtime_error("mesh3d must contain 'it' (triangle indices)");
  }
  
  Rcpp::IntegerMatrix it = Rcpp::as<Rcpp::IntegerMatrix>(mesh["it"]);
  data.triangle_count = it.ncol();
  
  // Build connectivity and offsets arrays
  data.connectivity.resize(data.triangle_count * 3);
  data.offsets.resize(data.triangle_count + 1);
  
  for (ttk::SimplexId i = 0; i < data.triangle_count; i++) {
    data.offsets[i] = i * 3;
    data.connectivity[i * 3]     = static_cast<ttk::LongSimplexId>(it(0, i)) - 1;
    data.connectivity[i * 3 + 1] = static_cast<ttk::LongSimplexId>(it(1, i)) - 1;
    data.connectivity[i * 3 + 2] = static_cast<ttk::LongSimplexId>(it(2, i)) - 1;
  }
  data.offsets[data.triangle_count] = data.triangle_count * 3;
  
  if (!mesh.containsElementNamed(scalar_field_name.c_str())) {
    throw std::runtime_error("Mesh does not contain scalar field: " + scalar_field_name);
  }
  
  Rcpp::NumericVector scalar = Rcpp::as<Rcpp::NumericVector>(mesh[scalar_field_name.c_str()]);
  if (scalar.size() != data.vertex_count) {
    throw std::runtime_error("Scalar field length does not match vertex count");
  }
  
  data.scalar_field.resize(data.vertex_count);
  for (ttk::SimplexId i = 0; i < data.vertex_count; i++) {
    data.scalar_field[i] = static_cast<float>(scalar[i]);
  }
  
  return data;
}


// [[Rcpp::export]]
Rcpp::List critical_points_cpp(const Rcpp::List& mesh,
                               const std::string& scalar_field_name,
                               bool compute_minima,
                               bool compute_maxima,
                               bool compute_saddle_points) {
  try {
    MeshData mesh_data = extract_mesh3d_data(mesh, scalar_field_name);
    
    // Create TTK triangulation
    ttk::Triangulation triangulation;
    triangulation.setInputPoints(mesh_data.vertex_count,
                                 mesh_data.vertices.data());
    
    // Use compatibility layer for setting cells
    ttk_compat::setInputCells(triangulation,
                              mesh_data.triangle_count,
                              mesh_data.connectivity,
                              mesh_data.offsets);
    
    // Use compatibility layer for preconditioning
    ttk_compat::preconditionTriangulation(triangulation);
    
    // Create order array using compatibility layer
    std::vector<ttk::SimplexId> order(mesh_data.vertex_count);
    ttk_compat::preconditionOrderArray(mesh_data.vertex_count,
                                       mesh_data.scalar_field.data(),
                                       order.data());
    
    // Setup critical points computation
    ttk::ScalarFieldCriticalPoints criticalPoints;
    std::vector<std::pair<ttk::SimplexId, char>> critical_points_output;
    
    criticalPoints.setVertexNumber(mesh_data.vertex_count);
    criticalPoints.setOutput(&critical_points_output);
    
    // Execute using compatibility layer
    int result = ttk_compat::executeCriticalPoints(criticalPoints,
                                                   order.data(),
                                                   &triangulation);
    
    if (result != 0) {
      throw std::runtime_error("TTK critical points computation failed");
    }
    
    // Process results
    std::vector<double> minima_x, minima_y, minima_z;
    std::vector<int> minima_ids;
    std::vector<double> minima_values;
    
    std::vector<double> maxima_x, maxima_y, maxima_z;
    std::vector<int> maxima_ids;
    std::vector<double> maxima_values;
    
    std::vector<double> saddle_x, saddle_y, saddle_z;
    std::vector<int> saddle_ids;
    std::vector<double> saddle_values;
    
    for (size_t i = 0; i < critical_points_output.size(); i++) {
      ttk::SimplexId vertex_id = critical_points_output[i].first;
      char cp_type = critical_points_output[i].second;
      
      double x = mesh_data.vertices[vertex_id * 3];
      double y = mesh_data.vertices[vertex_id * 3 + 1];
      double z = mesh_data.vertices[vertex_id * 3 + 2];
      double value = mesh_data.scalar_field[vertex_id];
      
      if (cp_type == 0) {
        if (compute_minima) {
          minima_ids.push_back(vertex_id + 1);
          minima_x.push_back(x);
          minima_y.push_back(y);
          minima_z.push_back(z);
          minima_values.push_back(value);
        }
      } else if (cp_type == 2) {
        if (compute_maxima) {
          maxima_ids.push_back(vertex_id + 1);
          maxima_x.push_back(x);
          maxima_y.push_back(y);
          maxima_z.push_back(z);
          maxima_values.push_back(value);
        }
      } else {
        if (compute_saddle_points) {
          saddle_ids.push_back(vertex_id + 1);
          saddle_x.push_back(x);
          saddle_y.push_back(y);
          saddle_z.push_back(z);
          saddle_values.push_back(value);
        }
      }
    }
    
    // Build result matrices
    int n_min = minima_ids.size();
    int n_max = maxima_ids.size();
    int n_sad = saddle_ids.size();
    
    Rcpp::NumericMatrix minima_mat(3, n_min);
    for (int i = 0; i < n_min; i++) {
      minima_mat(0, i) = minima_x[i];
      minima_mat(1, i) = minima_y[i];
      minima_mat(2, i) = minima_z[i];
    }
    
    Rcpp::NumericMatrix maxima_mat(3, n_max);
    for (int i = 0; i < n_max; i++) {
      maxima_mat(0, i) = maxima_x[i];
      maxima_mat(1, i) = maxima_y[i];
      maxima_mat(2, i) = maxima_z[i];
    }
    
    Rcpp::NumericMatrix saddle_mat(3, n_sad);
    for (int i = 0; i < n_sad; i++) {
      saddle_mat(0, i) = saddle_x[i];
      saddle_mat(1, i) = saddle_y[i];
      saddle_mat(2, i) = saddle_z[i];
    }
    
    return Rcpp::List::create(
      Rcpp::Named("minima") = Rcpp::List::create(
        Rcpp::Named("coordinates") = minima_mat,
        Rcpp::Named("vertex_ids") = Rcpp::wrap(minima_ids),
        Rcpp::Named("values") = Rcpp::wrap(minima_values)
      ),
      Rcpp::Named("maxima") = Rcpp::List::create(
        Rcpp::Named("coordinates") = maxima_mat,
        Rcpp::Named("vertex_ids") = Rcpp::wrap(maxima_ids),
        Rcpp::Named("values") = Rcpp::wrap(maxima_values)
      ),
      Rcpp::Named("saddles") = Rcpp::List::create(
        Rcpp::Named("coordinates") = saddle_mat,
        Rcpp::Named("vertex_ids") = Rcpp::wrap(saddle_ids),
        Rcpp::Named("values") = Rcpp::wrap(saddle_values)
      ),
      Rcpp::Named("scalar_field") = scalar_field_name,
      Rcpp::Named("total_vertices") = mesh_data.vertex_count,
      Rcpp::Named("total_triangles") = mesh_data.triangle_count
    );
    
  } catch (const std::exception& e) {
    Rcpp::stop(std::string("Error in ttk_critical_points: ") + e.what());
  } catch (...) {
    Rcpp::stop("Unknown error in ttk_critical_points");
  }
  
  return Rcpp::List();
}