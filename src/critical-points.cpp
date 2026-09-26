#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <stdexcept>

// TTK headers
#include <ttk/ScalarFieldCriticalPoints.h>
#include <ttk/Triangulation.h>
#include <ttk/Utils.h>

using namespace Rcpp;

/**
 * Convert R mesh3d object to TTK data structures
 */
struct MeshData {
  std::vector<float> vertices;
  std::vector<ttk::SimplexId> triangles;
  std::vector<float> scalar_field;
  ttk::SimplexId vertex_count;
  ttk::SimplexId triangle_count;
};

MeshData extract_mesh3d_data(const Rcpp::List& mesh, 
                             const std::string& scalar_field_name) {
  MeshData data;
  
  // Extract vertices (vb matrix: 4 x n, where first 3 rows are x,y,z)
  if (!mesh.containsElementNamed("vb")) {
    throw std::runtime_error("mesh3d must contain 'vb' (vertex matrix)");
  }
  
  Rcpp::NumericMatrix vb = Rcpp::as<Rcpp::NumericMatrix>(mesh["vb"]);
  if (vb.nrow() < 3) {
    throw std::runtime_error("Vertex matrix must have at least 3 rows (x,y,z)");
  }
  
  data.vertex_count = vb.ncol();
  data.vertices.resize(data.vertex_count * 3);
  
  for (ttk::SimplexId i = 0; i < data.vertex_count; i++) {
    data.vertices[i * 3]     = static_cast<float>(vb(0, i));
    data.vertices[i * 3 + 1] = static_cast<float>(vb(1, i));
    data.vertices[i * 3 + 2] = static_cast<float>(vb(2, i));
  }
  
  // Extract triangles (it matrix: 3 x n or 4 x n)
  if (!mesh.containsElementNamed("it")) {
    throw std::runtime_error("mesh3d must contain 'it' (triangle indices)");
  }
  
  Rcpp::IntegerMatrix it = Rcpp::as<Rcpp::IntegerMatrix>(mesh["it"]);
  data.triangle_count = it.ncol();
  data.triangles.resize(data.triangle_count * 3);
  
  for (ttk::SimplexId i = 0; i < data.triangle_count; i++) {
    // R uses 1-based indexing, TTK uses 0-based
    data.triangles[i * 3]     = it(0, i) - 1;
    data.triangles[i * 3 + 1] = it(1, i) - 1;
    data.triangles[i * 3 + 2] = it(2, i) - 1;
  }
  
  // Extract scalar field
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
Rcpp::List ttk_critical_points_cpp(const Rcpp::List& mesh,
                                   const std::string& scalar_field_name,
                                   bool compute_minima,
                                   bool compute_maxima,
                                   bool compute_saddle_points) {
  try {
    // 1. Extract mesh data
    MeshData mesh_data = extract_mesh3d_data(mesh, scalar_field_name);
    
    // 2. Create TTK triangulation
    ttk::Triangulation triangulation;
    triangulation.setInputPoints(mesh_data.vertex_count, 
                                 mesh_data.vertices.data());
    
    // Create cell array in TTK format
    std::vector<ttk::LongSimplexId> triangleSet;
    std::vector<ttk::LongSimplexId> triangleSetOff(mesh_data.triangle_count + 1);
    
    for (ttk::SimplexId i = 0; i < mesh_data.triangle_count; i++) {
      triangleSetOff[i] = i * 3;
    }
    triangleSetOff[mesh_data.triangle_count] = mesh_data.triangle_count * 3;
    
    triangleSet.resize(mesh_data.triangle_count * 3);
    for (size_t i = 0; i < mesh_data.triangles.size(); i++) {
      triangleSet[i] = mesh_data.triangles[i];
    }
    
#ifdef TTK_CELL_ARRAY_NEW
    triangulation.setInputCells(mesh_data.triangle_count,
                                triangleSet.data(),
                                triangleSetOff.data());
#else
    triangulation.setInputCells(mesh_data.triangle_count,
                                triangleSet.data());
#endif
    
    // 3. Precondition triangulation
    triangulation.preconditionEdges();
    if (triangulation.getDimensionality() == 2) {
      triangulation.preconditionTriangles();
    } else if (triangulation.getDimensionality() == 3) {
      triangulation.preconditionTriangles();
      triangulation.preconditionTetrahedrons();
    }
    
    // 4. Create order array (sorted indices based on scalar field)
    std::vector<ttk::SimplexId> order(mesh_data.vertex_count);
    ttk::preconditionOrderArray(mesh_data.vertex_count,
                                mesh_data.scalar_field.data(),
                                order.data(),
                                &triangulation);
    
    // 5. Setup critical points computation
    ttk::ScalarFieldCriticalPoints criticalPoints;
    
    // Output vector: pairs of (vertex_id, critical_point_type)
    // Type: 0=minimum, 1=saddle, 2=maximum (approximate)
    std::vector<std::pair<ttk::SimplexId, char>> critical_points_output;
    
    criticalPoints.setOutput(&critical_points_output);
    criticalPoints.setVertexNumber(mesh_data.vertex_count);
    
    // 6. Execute the algorithm
    int result = criticalPoints.execute<float>(
      mesh_data.scalar_field.data(),
      order.data(),
      &triangulation
    );
    
    if (result != 0) {
      throw std::runtime_error("TTK critical points computation failed");
    }
    
    // 7. Process results
    std::vector<double> minima_coords;
    std::vector<int> minima_ids;
    std::vector<double> minima_values;
    
    std::vector<double> maxima_coords;
    std::vector<int> maxima_ids;
    std::vector<double> maxima_values;
    
    std::vector<double> saddle_coords;
    std::vector<int> saddle_ids;
    std::vector<double> saddle_values;
    
    for (size_t i = 0; i < critical_points_output.size(); i++) {
      ttk::SimplexId vertex_id = critical_points_output[i].first;
      char cp_type = critical_points_output[i].second;
      
      // Get vertex coordinates
      double x = mesh_data.vertices[vertex_id * 3];
      double y = mesh_data.vertices[vertex_id * 3 + 1];
      double z = mesh_data.vertices[vertex_id * 3 + 2];
      double value = mesh_data.scalar_field[vertex_id];
      
      // Classify based on type
      // Note: TTK type encoding may vary, using heuristic based on value
      if (cp_type == 0 || cp_type == 'M') {
        // Local minimum
        if (compute_minima) {
          minima_ids.push_back(vertex_id + 1); // Convert to 1-based
          minima_coords.push_back(x);
          minima_coords.push_back(y);
          minima_coords.push_back(z);
          minima_values.push_back(value);
        }
      } else if (cp_type == 2 || cp_type == 'X') {
        // Local maximum
        if (compute_maxima) {
          maxima_ids.push_back(vertex_id + 1);
          maxima_coords.push_back(x);
          maxima_coords.push_back(y);
          maxima_coords.push_back(z);
          maxima_values.push_back(value);
        }
      } else {
        // Saddle point
        if (compute_saddle_points) {
          saddle_ids.push_back(vertex_id + 1);
          saddle_coords.push_back(x);
          saddle_coords.push_back(y);
          saddle_coords.push_back(z);
          saddle_values.push_back(value);
        }
      }
    }
    
    // 8. Build result matrices
    Rcpp::NumericMatrix minima_mat(3, minima_ids.size());
    Rcpp::NumericMatrix maxima_mat(3, maxima_ids.size());
    Rcpp::NumericMatrix saddle_mat(3, saddle_ids.size());
    
    for (size_t i = 0; i < minima_ids.size(); i++) {
      minima_mat(0, i) = minima_coords[i * 3];
      minima_mat(1, i) = minima_coords[i * 3 + 1];
      minima_mat(2, i) = minima_coords[i * 3 + 2];
    }
    
    for (size_t i = 0; i < maxima_ids.size(); i++) {
      maxima_mat(0, i) = maxima_coords[i * 3];
      maxima_mat(1, i) = maxima_coords[i * 3 + 1];
      maxima_mat(2, i) = maxima_coords[i * 3 + 2];
    }
    
    for (size_t i = 0; i < saddle_ids.size(); i++) {
      saddle_mat(0, i) = saddle_coords[i * 3];
      saddle_mat(1, i) = saddle_coords[i * 3 + 1];
      saddle_mat(2, i) = saddle_coords[i * 3 + 2];
    }
    
    // 9. Return results as R list
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
  
  return Rcpp::List(); // Never reached
}