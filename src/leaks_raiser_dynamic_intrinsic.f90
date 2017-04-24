module dynamic_intrinsic_type_m
   implicit none
   type :: dynamic_intrinsic_type_t
      integer, allocatable :: x
   contains
      generic :: assignment(=) => assign_dynamic_intrinsic_type
      generic :: operator(+) => add_dynamic_intrinsic_type
      procedure, pass(lhs) :: assign_dynamic_intrinsic_type
      procedure, pass(lhs) :: add_dynamic_intrinsic_type
   endtype dynamic_intrinsic_type_t
contains
   subroutine assign_dynamic_intrinsic_type(lhs, rhs)
      ! Operator `=`.
      class(dynamic_intrinsic_type_t), intent(inout) :: lhs
      class(dynamic_intrinsic_type_t), intent(in)    :: rhs
      if (allocated(rhs%x)) then
        if (.not.allocated(lhs%x)) allocate(lhs%x)
        lhs%x = rhs%x
      endif
   endsubroutine assign_dynamic_intrinsic_type

   function add_dynamic_intrinsic_type(lhs, rhs) result(res)
      ! Operator `+`.
      class(dynamic_intrinsic_type_t), intent(in)  :: lhs
      class(dynamic_intrinsic_type_t), intent(in)  :: rhs
      class(dynamic_intrinsic_type_t), allocatable :: res
      allocate (dynamic_intrinsic_type_t :: res)
      if (allocated(lhs%x).and.allocated(rhs%x)) then
        allocate(res%x)
        res%x = lhs%x + rhs%x
      endif
   endfunction add_dynamic_intrinsic_type
endmodule dynamic_intrinsic_type_m

program leaks_raiser
   use dynamic_intrinsic_type_m
   implicit none
   type(dynamic_intrinsic_type_t) :: a
   type(dynamic_intrinsic_type_t) :: b

   allocate(a%x)
   a%x = 1
   b = a
   b = a + b ! here the `+` operator could generate memory leaks
endprogram leaks_raiser
