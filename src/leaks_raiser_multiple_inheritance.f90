module ancestor_m
implicit none

type, abstract :: ancestor_t
   integer, allocatable :: workaround
   contains
      generic :: assignment(=) => assign_
      generic :: operator(+) => add_
      procedure(assign_interface), pass(lhs), deferred :: assign_
      procedure(add_interface),    pass(lhs), deferred :: add_
endtype ancestor_t

abstract interface
   subroutine assign_interface(lhs, rhs)
   ! Operator `=`.
   import :: ancestor_t
   class(ancestor_t), intent(inout) :: lhs
   class(ancestor_t), intent(in)    :: rhs
   endsubroutine assign_interface

   function add_interface(lhs, rhs) result(opr)
   ! Operator `+`.
   import :: ancestor_t
   class(ancestor_t), intent(in)  :: lhs
   class(ancestor_t), intent(in)  :: rhs
   class(ancestor_t), allocatable :: opr
   endfunction add_interface
endinterface
endmodule ancestor_m

module parent_m
use ancestor_m
implicit none

type, extends(ancestor_t), abstract :: parent_t
endtype parent_t
endmodule parent_m

module child_m
use ancestor_m
use parent_m
implicit none

type, extends(parent_t) :: child_t
   integer              :: x
   integer, allocatable :: y
   contains
      procedure, pass(lhs) :: assign_
      procedure, pass(lhs) :: add_
endtype child_t

contains
   subroutine assign_(lhs, rhs)
   ! Operator `=`.
   class(child_t),    intent(inout) :: lhs
   class(ancestor_t), intent(in)    :: rhs

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
   class(child_t),    intent(in)  :: lhs
   class(ancestor_t), intent(in)  :: rhs
   class(ancestor_t), allocatable :: opr

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
