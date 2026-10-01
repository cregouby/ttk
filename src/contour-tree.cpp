#include <Rcpp.h>

#include <ttk/base/ContourTree.h>

#include "ttk_compat.h"

#include <vector>
#include <algorithm>
#include <stdexcept>
#include <utility>

using namespace Rcpp;

namespace {

void addUniqueNeighbor(std::vector<int>& neighbors, int vertex) {
  if (std::find(neighbors.begin(), neighbors.end(), vertex) == neighbors.end()) {
    neighbors.push_back(vertex);
  }
}

} // namespace

// [[Rcpp::export]]
Rcpp::List contour_tree_cpp(const Rcpp::List& mesh,
                            const std::string& scalar_field_name) {
  try {
    // Extract vertex matrix
    if (!mesh.containsElementNamed("vb")) {
      throw std::runtime_error("mesh3d must contain 'vb' (vertex matrix)");
    }
    
    Rcpp::NumericMatrix vb = Rcpp::as<Rcpp::NumericMatrix>(mesh["vb"]);
    
    if (vb.nrow() < 3) {
      throw std::runtime_error("Vertex matrix must have at least 3 rows");
    }
    
    const ttk::SimplexId vertex_count = vb.ncol();
    
    if (vertex_count == 0) {
      return Rcpp::List::create(
        Rcpp::Named("nodes") = Rcpp::List::create(
          Rcpp::Named("ids") = Rcpp::IntegerVector(0),
          Rcpp::Named("values") = Rcpp::NumericVector(0)
        ),
        Rcpp::Named("edges") = Rcpp::List::create(
          Rcpp::Named("source") = Rcpp::IntegerVector(0),
          Rcpp::Named("target") = Rcpp::IntegerVector(0)
        ),
        Rcpp::Named("n_nodes") = 0,
        Rcpp::Named("n_edges") = 0,
        Rcpp::Named("scalar_field") = scalar_field_name,
        Rcpp::Named("total_vertices") = 0,
        Rcpp::Named("total_triangles") = 0,
        Rcpp::Named("implemented") = true
      );
    }
    
    std::vector<float> vertices(static_cast<std::size_t>(vertex_count) * 3);
    
    for (ttk::SimplexId i = 0; i < vertex_count; ++i) {
      vertices[static_cast<std::size_t>(i) * 3 + 0] =
        static_cast<float>(vb(0, i));
      vertices[static_cast<std::size_t>(i) * 3 + 1] =
        static_cast<float>(vb(1, i));
      vertices[static_cast<std::size_t>(i) * 3 + 2] =
        static_cast<float>(vb(2, i));
    }
    
    // Extract triangle matrix
    if (!mesh.containsElementNamed("it")) {
      throw std::runtime_error("mesh3d must contain 'it' (triangle indices)");
    }
    
    Rcpp::IntegerMatrix it = Rcpp::as<Rcpp::IntegerMatrix>(mesh["it"]);
    const ttk::SimplexId triangle_count = it.ncol();
    
    std::vector<ttk::LongSimplexId> connectivity(
        static_cast<std::size_t>(triangle_count) * 3
    );
    
    std::vector<ttk::LongSimplexId> offsets(
        static_cast<std::size_t>(triangle_count) + 1
    );
    
    for (ttk::SimplexId i = 0; i < triangle_count; ++i) {
      offsets[static_cast<std::size_t>(i)] =
        static_cast<ttk::LongSimplexId>(i) * 3;
      
      connectivity[static_cast<std::size_t>(i) * 3 + 0] =
        static_cast<ttk::LongSimplexId>(it(0, i)) - 1;
      connectivity[static_cast<std::size_t>(i) * 3 + 1] =
        static_cast<ttk::LongSimplexId>(it(1, i)) - 1;
      connectivity[static_cast<std::size_t>(i) * 3 + 2] =
        static_cast<ttk::LongSimplexId>(it(2, i)) - 1;
    }
    
    offsets[static_cast<std::size_t>(triangle_count)] =
      static_cast<ttk::LongSimplexId>(triangle_count) * 3;
    
    // Extract scalar field
    if (!mesh.containsElementNamed(scalar_field_name.c_str())) {
      throw std::runtime_error(
          "Mesh does not contain scalar field: " + scalar_field_name
      );
    }
    
    Rcpp::NumericVector scalar_r =
      Rcpp::as<Rcpp::NumericVector>(mesh[scalar_field_name.c_str()]);
    
    if (scalar_r.size() != vertex_count) {
      throw std::runtime_error(
          "Scalar field length does not match vertex count"
      );
    }
    
    std::vector<float> scalar_field(static_cast<std::size_t>(vertex_count));
    std::vector<double> vertex_scalars(static_cast<std::size_t>(vertex_count));
    
    for (ttk::SimplexId i = 0; i < vertex_count; ++i) {
      const float value = static_cast<float>(scalar_r[i]);
      scalar_field[static_cast<std::size_t>(i)] = value;
      vertex_scalars[static_cast<std::size_t>(i)] =
        static_cast<double>(value);
    }
    
    // Build TTK triangulation
    ttk::Triangulation triangulation;
    
    triangulation.setInputPoints(
      vertex_count,
      vertices.data()
    );
    
    ttk_compat::setInputCells(
      triangulation,
      triangle_count,
      connectivity,
      offsets
    );
    
    ttk_compat::preconditionTriangulation(triangulation);
    
    // Build order array, also called SoS offsets in ContourTree API
    std::vector<ttk::SimplexId> order(static_cast<std::size_t>(vertex_count));
    
    ttk_compat::preconditionOrderArray(
      vertex_count,
      scalar_field.data(),
      order.data()
    );
    
    std::vector<int> vertex_sos_offsets(static_cast<std::size_t>(vertex_count));
    
    for (ttk::SimplexId i = 0; i < vertex_count; ++i) {
      vertex_sos_offsets[static_cast<std::size_t>(i)] =
        static_cast<int>(order[static_cast<std::size_t>(i)]);
    }
    
    // Build vertex neighbor lists from triangles
    std::vector<std::vector<int>> vertex_neighbors(
        static_cast<std::size_t>(vertex_count)
    );
    
    for (ttk::SimplexId i = 0; i < triangle_count; ++i) {
      const int a = static_cast<int>(
        connectivity[static_cast<std::size_t>(i) * 3 + 0]
      );
      const int b = static_cast<int>(
        connectivity[static_cast<std::size_t>(i) * 3 + 1]
      );
      const int c = static_cast<int>(
        connectivity[static_cast<std::size_t>(i) * 3 + 2]
      );
      
      addUniqueNeighbor(vertex_neighbors[static_cast<std::size_t>(a)], b);
      addUniqueNeighbor(vertex_neighbors[static_cast<std::size_t>(a)], c);
      
      addUniqueNeighbor(vertex_neighbors[static_cast<std::size_t>(b)], a);
      addUniqueNeighbor(vertex_neighbors[static_cast<std::size_t>(b)], c);
      
      addUniqueNeighbor(vertex_neighbors[static_cast<std::size_t>(c)], a);
      addUniqueNeighbor(vertex_neighbors[static_cast<std::size_t>(c)], b);
    }
    
    // Compute minima and maxima using the SoS order
    std::vector<int> minimum_list;
    std::vector<int> maximum_list;
    
    for (ttk::SimplexId v = 0; v < vertex_count; ++v) {
      const std::size_t idx = static_cast<std::size_t>(v);
      const int rank_v = vertex_sos_offsets[idx];
      
      const std::vector<int>& neighbors = vertex_neighbors[idx];
      
      bool is_minimum = true;
      bool is_maximum = true;
      
      for (std::size_t k = 0; k < neighbors.size(); ++k) {
        const int u = neighbors[k];
        const int rank_u = vertex_sos_offsets[static_cast<std::size_t>(u)];
        
        if (rank_u < rank_v) {
          is_minimum = false;
        }
        
        if (rank_u > rank_v) {
          is_maximum = false;
        }
      }
      
      // Isolated vertices are treated as both minimum and maximum
      if (neighbors.empty()) {
        minimum_list.push_back(static_cast<int>(v));
        maximum_list.push_back(static_cast<int>(v));
      } else {
        if (is_minimum) {
          minimum_list.push_back(static_cast<int>(v));
        }
        
        if (is_maximum) {
          maximum_list.push_back(static_cast<int>(v));
        }
      }
    }
    
    // Ensure at least one minimum and one maximum for degenerate cases
    if (minimum_list.empty()) {
      int best_vertex = 0;
      int best_rank = vertex_sos_offsets[0];
      
      for (ttk::SimplexId v = 1; v < vertex_count; ++v) {
        const int rank_v = vertex_sos_offsets[static_cast<std::size_t>(v)];
        
        if (rank_v < best_rank) {
          best_rank = rank_v;
          best_vertex = static_cast<int>(v);
        }
      }
      
      minimum_list.push_back(best_vertex);
    }
    
    if (maximum_list.empty()) {
      int best_vertex = 0;
      int best_rank = vertex_sos_offsets[0];
      
      for (ttk::SimplexId v = 1; v < vertex_count; ++v) {
        const int rank_v = vertex_sos_offsets[static_cast<std::size_t>(v)];
        
        if (rank_v > best_rank) {
          best_rank = rank_v;
          best_vertex = static_cast<int>(v);
        }
      }
      
      maximum_list.push_back(best_vertex);
    }
    
    // Build contour tree
    ttk::ContourTree contour_tree;
    
    contour_tree.setNumberOfVertices(static_cast<int>(vertex_count));
    contour_tree.setVertexScalars(&vertex_scalars);
    contour_tree.setVertexSoSoffsets(&vertex_sos_offsets);
    contour_tree.setTriangulation(&triangulation);
    contour_tree.setMinimumList(minimum_list);
    contour_tree.setMaximumList(maximum_list);
    
    // This setter exists in the installed header.
    // If a future TTK version removes it, this line can be commented out.
    contour_tree.setVertexNeighbors(&vertex_neighbors);
    
    const int build_status = contour_tree.build();
    
    if (build_status != 0) {
      throw std::runtime_error("TTK contour tree build failed");
    }
    
    // Extract nodes
    const int n_nodes = contour_tree.getNumberOfNodes();
    
    std::vector<int> node_ids;
    std::vector<double> node_values;
    
    node_ids.reserve(static_cast<std::size_t>(n_nodes));
    node_values.reserve(static_cast<std::size_t>(n_nodes));
    
    for (int node_id = 0; node_id < n_nodes; ++node_id) {
      const ttk::Node* node = contour_tree.getNode(node_id);
      
      if (node == nullptr) {
        continue;
      }
      
      const int vertex_id = node->getVertexId();
      
      if (vertex_id < 0 || vertex_id >= static_cast<int>(vertex_count)) {
        continue;
      }
      
      node_ids.push_back(vertex_id + 1);
      node_values.push_back(
        vertex_scalars[static_cast<std::size_t>(vertex_id)]
      );
    }
    
    // Extract arcs as edges between tree nodes
    const int n_arcs = contour_tree.getNumberOfArcs();
    
    std::vector<int> edge_source;
    std::vector<int> edge_target;
    
    edge_source.reserve(static_cast<std::size_t>(n_arcs));
    edge_target.reserve(static_cast<std::size_t>(n_arcs));
    
    for (int arc_id = 0; arc_id < n_arcs; ++arc_id) {
      const ttk::Arc* arc = contour_tree.getArc(arc_id);
      
      if (arc == nullptr) {
        continue;
      }
      
      const int down_node_id = arc->getDownNodeId();
      const int up_node_id = arc->getUpNodeId();
      
      if (down_node_id < 0 || up_node_id < 0) {
        continue;
      }
      
      if (down_node_id >= n_nodes || up_node_id >= n_nodes) {
        continue;
      }
      
      edge_source.push_back(down_node_id + 1);
      edge_target.push_back(up_node_id + 1);
    }
    
    return Rcpp::List::create(
      Rcpp::Named("nodes") = Rcpp::List::create(
        Rcpp::Named("ids") = Rcpp::wrap(node_ids),
        Rcpp::Named("values") = Rcpp::wrap(node_values)
      ),
      Rcpp::Named("edges") = Rcpp::List::create(
        Rcpp::Named("source") = Rcpp::wrap(edge_source),
        Rcpp::Named("target") = Rcpp::wrap(edge_target)
      ),
      Rcpp::Named("n_nodes") = static_cast<int>(node_ids.size()),
      Rcpp::Named("n_edges") = static_cast<int>(edge_source.size()),
      Rcpp::Named("scalar_field") = scalar_field_name,
      Rcpp::Named("total_vertices") = vertex_count,
      Rcpp::Named("total_triangles") = triangle_count,
      Rcpp::Named("implemented") = true
    );
    
  } catch (const std::exception& e) {
    Rcpp::stop(std::string("Error in contour_tree: ") + e.what());
  } catch (...) {
    Rcpp::stop("Unknown error in contour_tree");
  }
  
  return Rcpp::List();
}