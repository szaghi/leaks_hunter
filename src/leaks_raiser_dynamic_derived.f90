module dynamic_derived_type_m
   implicit none
   type :: derived_t
      integer :: x
   endtype derived_t
   type :: dynamic_derived_type_t
      type(derived_t), allocatable :: x
   contains
      generic :: assignment(=) => assign_dynamic_derived_type
      generic :: operator(+) => add_dynamic_derived_type
      procedure, pass(lhs) :: assign_dynamic_derived_type
      procedure, pass(lhs) :: add_dynamic_derived_type
   endtype dynamic_derived_type_t
contains
   subroutine assign_dynamic_derived_type(lhs, rhs)
      ! Operator `=`.
      class(dynamic_derived_type_t), intent(inout) :: lhs
      class(dynamic_derived_type_t), intent(in)    :: rhs
      if (allocated(rhs%x)) then
        if (.not.allocated(lhs%x)) allocate(lhs%x)
        lhs%x%x = rhs%x%x
      endif
   endsubroutine assign_dynamic_derived_type

   function add_dynamic_derived_type(lhs, rhs) result(res)
      ! Operator `+`.
      class(dynamic_derived_type_t), intent(in)  :: lhs
      class(dynamic_derived_type_t), intent(in)  :: rhs
      class(dynamic_derived_type_t), allocatable :: res
      allocate (dynamic_derived_type_t :: res)
      if (allocated(lhs%x).and.allocated(rhs%x)) then
        allocate(res%x)
        res%x%x = lhs%x%x + rhs%x%x
      endif
   endfunction add_dynamic_derived_type
endmodule dynamic_derived_type_m

program leaks_raiser
   use dynamic_derived_type_m
   implicit none
   type(dynamic_derived_type_t) :: a
   type(dynamic_derived_type_t) :: b

   allocate(a%x)
   a%x%x = 1
   b = a
   b = a + b ! here the `+` operator could generate memory leaks
endprogram leaks_raiser
