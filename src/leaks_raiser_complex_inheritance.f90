module inherit_type_m
   implicit none
   type :: derived_t
      integer :: x
   contains
      generic :: assignment(=) => assign_derived_type
      generic :: operator(+) => add_derived_type
      procedure, pass(lhs) :: assign_derived_type
      procedure, pass(lhs) :: add_derived_type
   endtype derived_t

   type :: inherit_type_t
      type(derived_t), allocatable :: x
   contains
      generic :: assignment(=) => assign_inherit_type
      generic :: operator(+) => add_inherit_type
      procedure, pass(lhs) :: assign_inherit_type
      procedure, pass(lhs) :: add_inherit_type
   endtype inherit_type_t
contains
   subroutine assign_derived_type(lhs, rhs)
      ! Operator `=`.
      class(derived_t), intent(inout) :: lhs
      class(derived_t), intent(in)    :: rhs
      lhs%x = rhs%x
   endsubroutine assign_derived_type

   function add_derived_type(lhs, rhs) result(res)
      ! Operator `+`.
      class(derived_t), intent(in)  :: lhs
      class(derived_t), intent(in)  :: rhs
      class(derived_t), allocatable :: res
      allocate (derived_t :: res)
      res%x = lhs%x + rhs%x
   endfunction add_derived_type

   subroutine assign_inherit_type(lhs, rhs)
      ! Operator `=`.
      class(inherit_type_t), intent(inout) :: lhs
      class(inherit_type_t), intent(in)    :: rhs
      if (allocated(rhs%x)) then
        if (.not.allocated(lhs%x)) allocate(lhs%x)
        lhs%x = rhs%x
      endif
   endsubroutine assign_inherit_type

   function add_inherit_type(lhs, rhs) result(res)
      ! Operator `+`.
      class(inherit_type_t), intent(in)  :: lhs
      class(inherit_type_t), intent(in)  :: rhs
      class(inherit_type_t), allocatable :: res
      allocate (inherit_type_t :: res)
      if (allocated(lhs%x).and.allocated(rhs%x)) then
        allocate(res%x)
        res%x = lhs%x + rhs%x
      endif
   endfunction add_inherit_type
endmodule inherit_type_m

program leaks_raiser
   use inherit_type_m
   implicit none
   type(inherit_type_t) :: a
   type(inherit_type_t) :: b

   allocate(a%x)
   a%x = derived_t(1)
   b = a
   b = a + b ! here the `+` operator could generate memory leaks
endprogram leaks_raiser
