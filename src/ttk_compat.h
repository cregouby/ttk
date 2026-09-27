#ifndef TTK_COMPAT_H
#define TTK_COMPAT_H

// TTK compatibility layer
// Handles API differences between TTK versions

#include <ttk/base/Triangulation.h>
#include <ttk/base/ScalarFieldCriticalPoints.h>
#include <ttk/base/PersistenceDiagram.h>

#include <vector>
#include <stdexcept>

namespace ttk_compat {

// Set input cells on a triangulation
// Automatically detects the correct API at compile time
inline void setInputCells(ttk::Triangulation& triangulation,
                          ttk::SimplexId cellNumber,
                          const std::vector<ttk::LongSimplexId>& connectivity,
                          const std::vector<ttk::LongSimplexId>& offsets) {
  
#ifdef TTK_CELL_ARRAY_NEW
  // New API (TTK >= 1.2.0): separate connectivity and offsets arrays
  triangulation.setInputCells(cellNumber,
                              connectivity.data(),
                              offsets.data());
#else
  // Old API (TTK < 1.2.0): flat layout [n, v0, v1, ..., n, v0, v1, ...]
  std::vector<ttk::LongSimplexId> flatLayout;
  flatLayout.reserve(offsets[cellNumber] + cellNumber);
  
  for (ttk::SimplexId i = 0; i < cellNumber; i++) {
    ttk::LongSimplexId start = offsets[i];
    ttk::LongSimplexId end = offsets[i + 1];
    ttk::LongSimplexId nVerts = end - start;
    flatLayout.push_back(nVerts);
    for (ttk::LongSimplexId j = start; j < end; j++) {
      flatLayout.push_back(connectivity[j]);
    }
  }
  
  triangulation.setInputCells(cellNumber, flatLayout.data());
#endif
}

// Precondition order array
inline void preconditionOrderArray(ttk::SimplexId vertexCount,
                                   const float* scalarField,
                                   ttk::SimplexId* order) {
  ttk::preconditionOrderArray(
    static_cast<size_t>(vertexCount),
    scalarField,
    order,
    1  // single thread for safety
  );
}

// Precondition the triangulation for computation
inline void preconditionTriangulation(ttk::Triangulation& triangulation) {
  triangulation.preconditionEdges();
  
  int dim = triangulation.getDimensionality();
  if (dim == 2) {
    triangulation.preconditionTriangles();
  } else if (dim == 3) {
    triangulation.preconditionTriangles();
    triangulation.preconditionVertexEdges();
  }
}

// Execute scalar field critical points
inline int executeCriticalPoints(
    ttk::ScalarFieldCriticalPoints& criticalPoints,
    const ttk::SimplexId* order,
    const ttk::Triangulation* triangulation) {
  
  return criticalPoints.execute(order, triangulation);
}

// Execute persistence diagram computation
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
    0,  // offset
    order,
    triangulation
  );
}

} // namespace ttk_compat

#endif // TTK_COMPAT_H