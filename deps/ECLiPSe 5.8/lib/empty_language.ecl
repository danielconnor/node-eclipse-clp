/* "empty" language module for use with eplex modules */

:- module(empty_language,[],[sepia_kernel]).

:- reexport comment / 2 from sepia_kernel.
:- reexport (local) / 1 from sepia_kernel.
:- reexport (export) / 1 from sepia_kernel.
:- reexport (import) / 1 from sepia_kernel.
:- reexport (writeln) / 2 from sepia_kernel.
:- reexport ensure_loaded / 1 from sepia_kernel.
:- reexport current_module / 1 from sepia_kernel.
:- reexport true / 0 from sepia_kernel.
:- reexport create_module / 3 from sepia_kernel.
:- reexport (->) / 2 from sepia_kernel.
:- reexport (;) / 2 from sepia_kernel.
