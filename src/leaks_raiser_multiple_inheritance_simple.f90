module parent_m
implicit none

type :: parent_t
   integer, allocatable :: workaround
endtype parent_t
endmodule parent_m

module child_m
use parent_m
implicit none

type, extends(parent_t) :: child_t
   integer              :: x
   integer, allocatable :: y
   contains
      generic :: assignment(=) => assign_
      generic :: operator(+) => add_
      procedure, pass(lhs) :: assign_
      procedure, pass(lhs) :: add_
endtype child_t

contains
   subroutine assign_(lhs, rhs)
   ! Operator `=`.
   class(child_t),  intent(inout) :: lhs
   class(parent_t), intent(in)    :: rhs

   select type(rhs)
   class is(child_t)
      lhs%x = rhs%x
      if (allocated(rhs%y)) then
        if (.not.allocated(lhs%y)) allocate(lhs%y)
        lhs%y = rhs%y
      endif
   endselect
   endsubroutine assign_

   function add_(lhs, rhs) result(opr)
   ! Operator `+`.
   class(child_t),  intent(in)  :: lhs
   class(parent_t), intent(in)  :: rhs
   class(parent_t), allocatable :: opr

   allocate(child_t :: opr)
   select type(opr)
   class is(child_t)
      select type(rhs)
      class is(child_t)
         opr%x = lhs%x + rhs%x
         if (allocated(lhs%y).and.allocated(rhs%y)) then
           allocate(opr%y)
           opr%y = lhs%y + rhs%y
         endif
      endselect
   endselect
   endfunction add_
endmodule child_m

program leaks_raiser
   use child_m
   implicit none
   type(child_t) :: a
   type(child_t) :: b

   allocate(a%y)
   a%x = 1
   a%y = 2
   b = a
   b = a + b ! here the `+` operator could generate memory leaks
endprogram leaks_raiser
