#include "cppdefs.h"
module ecosys_constants
#if defined BEC

  use kinds_mod, only : int_kind

  implicit none

  public
#ifndef FE_TAG
  integer(int_kind), parameter :: ecosys_tracer_cnt = 27
#else
  integer(int_kind), parameter :: ecosys_tracer_cnt = 70
#endif

#endif
end module ecosys_constants
