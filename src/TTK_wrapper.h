#ifndef TTK_WRAPPER_H
#define TTK_WRAPPER_H

#include <Rcpp.h>
#include <ttk/base/TTKPeriodicCube.h>

namespace ttk_wrapper {

struct MeshData {
  std::vector<double> vertices;
  std::vector<int> triangles;
  std::vector<double> scalar_field;
};

MeshData convert_mesh3d_to_ttk(const Rcpp::List& mesh,
                               const std::string& scalar_field_name);

}

#endif