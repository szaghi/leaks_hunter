module static_intrinsic_type_m
   implicit none
   type :: static_intrinsic_type_t
      integer :: x
   contains
      generic :: assignment(=) => assign_static_intrinsic_type
      generic :: operator(+) => add_static_intrinsic_type
      procedure, pass(lhs) :: assign_static_intrinsic_type
      procedure, pass(lhs) :: add_static_intrinsic_type
   endtype static_intrinsic_type_t
contains
   subroutine assign_static_intrinsic_type(lhs, rhs)
      ! Operator `=`.
      class(static_intrinsic_type_t), intent(inout) :: lhs
      class(static_intrinsic_type_t), intent(in)    :: rhs
      lhs%x = rhs%x
   endsubroutine assign_static_intrinsic_type

   function add_static_intrinsic_type(lhs, rhs) result(res)
      ! Operator `+`.
      class(static_intrinsic_type_t), intent(in)  :: lhs
      class(static_intrinsic_type_t), intent(in)  :: rhs
      class(static_intrinsic_type_t), allocatable :: res
      allocate (static_intrinsic_type_t :: res)
      res%x = lhs%x + rhs%x
   endfunction add_static_intrinsic_type
endmodule static_intrinsic_type_m

program leaks_raiser
   use static_intrinsic_type_m
   implicit none
   type(static_intrinsic_type_t) :: a
   type(static_intrinsic_type_t) :: b

   a%x = 1
   b = a
   b = a + b ! here the `+` operator could generate memory leaks
endprogram leaks_raiser
