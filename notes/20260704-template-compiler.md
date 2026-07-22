# Template compiler

*2026-07-04*

I'm imagining rendering these templates on a web server as web pages. The graph
of templates shouldn't be repeatedly type checked if nothing has changed.
So there should be a database of compiled templates that are guaranteed to
be correct with respect to each other.

If we assume access to a file system, then we can have a 1-to-1 correspondence
between template files and compiled templates. This means that template indexing
doesn't need to be implemented. This approach also allows templates to be stored
as binary blobs in a database.

Templates should be compiled to bytecode that gets interpreted by a simple
virtual machine. The purpose of the VM is to write bytes to some output buffer.
In addition to the main output buffer, there may be auxiliary buffers for
holding arguments to a parent template that aren't ready to be substituted.
