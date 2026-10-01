#ifndef TTK_COMPAT_H
#define TTK_COMPAT_H

// TTK compatibility layer
// Handles API differences between TTK versions

#include <ttk/base/Triangulation.h>
#include <ttk/base/ScalarFieldCriticalPoints.h>
#include <ttk/base/PersistenceDiagram.h>

#include <vector>
#include <cstddef>

namespace ttk_compat {

inline void setInputCells(ttk::Triangulation& triangulation,
                          ttk::SimplexId cellNumber,
                          const std::vector<ttk::LongSimplexId>& connectivity,
                          const std::vector<ttk::LongSimplexId>& offsets) {
#ifdef TTK_CELL_ARRAY_NEW
  triangulation.setInputCells(cellNumber,
                              connectivity.data(),
                              offsets.data());
#else
  std::vector<ttk::LongSimplexId> flatLayout;
  flatLayout.reserve(static_cast<std::size_t>(offsets[cellNumber]) +
    static_cast<std::size_t>(cellNumber));
  
  for (ttk::SimplexId i = 0; i < cellNumber; ++i) {
    ttk::LongSimplexId start = offsets[i];
    ttk::LongSimplexId end = offsets[i + 1];
    ttk::LongSimplexId nVerts = end - start;
    
    flatLayout.push_back(nVerts);
    
    for (ttk::LongSimplexId j = start; j < end; ++j) {
      flatLayout.push_back(connectivity[j]);
    }
  }
  
  triangulation.setInputCells(cellNumber, flatLayout.data());
#endif
}

inline void preconditionOrderArray(ttk::SimplexId vertexCount,
                                   const float* scalarField,
                                   ttk::SimplexId* order) {
  ttk::preconditionOrderArray(
    static_cast<std::size_t>(vertexCount),
    scalarField,
    order,
    1
  );
}

inline void preconditionTriangulation(ttk::Triangulation& triangulation) {
  triangulation.preconditionEdges();
  
  const int dim = triangulation.getDimensionality();
  
  if (dim == 2) {
    triangulation.preconditionTriangles();
  } else if (dim == 3) {
    triangulation.preconditionTriangles();
    triangulation.preconditionVertexEdges();
  }
}

inline int executeCriticalPoints(
    ttk::ScalarFieldCriticalPoints& criticalPoints,
    const ttk::SimplexId* order,
    const ttk::Triangulation* triangulation) {
  
  return criticalPoints.execute(order, triangulation);
}

inline int executePersistenceDiagram(
    ttk::PersistenceDiagram& diagram,
    std::vector<ttk::PersistencePair>& output,
    const float* scalars,
    const ttk::SimplexId* order,
    ttk::Triangulation* triangulation) {
  
  diagram.preconditionTriangulation(triangulation);
  
  return diagram.execute(
    output,
    scalars,
    0,
    order,
    triangulation
  );
}

} // namespace ttk_compat

#endif // TTK_COMPAT_H