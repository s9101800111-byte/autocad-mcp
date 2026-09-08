"""Drawing analysis that runs on a DXF file, independent of any backend.

The `space` tool wraps these. They are plain functions over a file path so
they can also be called from a script or a test without AutoCAD running —
which is the point: pulling tens of thousands of entities through the LISP
IPC is what the entity docstring already warns against.
"""
