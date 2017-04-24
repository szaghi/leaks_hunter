module static_derived_type_m
   implicit none
   type :: derived_t
      integer :: x
   endtype derived_t
   type :: static_derived_type_t
      type(derived_t) :: x
   contains
      generic :: assignment(=) => assign_static_derived_type
      generic :: operator(+) => add_static_derived_type
      procedure, pass(lhs) :: assign_static_derived_type
      procedure, pass(lhs) :: add_static_derived_type
   endtype static_derived_type_t
contains
   subroutine assign_static_derived_type(lhs, rhs)
      ! Operator `=`.
      class(static_derived_type_t), intent(inout) :: lhs
      class(static_derived_type_t), intent(in)    :: rhs
      lhs%x%x = rhs%x%x
   endsubroutine assign_static_derived_type

   function add_static_derived_type(lhs, rhs) result(res)
      ! Operator `+`.
      class(static_derived_type_t), intent(in)  :: lhs
      class(static_derived_type_t), intent(in)  :: rhs
      class(static_derived_type_t), allocatable :: res
      allocate (static_derived_type_t :: res)
      res%x%x = lhs%x%x + rhs%x%x
   endfunction add_static_derived_type
endmodule static_derived_type_m

program leaks_raiser
   use static_derived_type_m
   implicit none
   type(static_derived_type_t) :: a
   type(static_derived_type_t) :: b

   a%x%x = 1
   b = a
   b = a + b ! here the `+` operator could generate memory leaks
endprogram leaks_raiser
