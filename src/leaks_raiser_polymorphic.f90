module polymorphic_type_m
   implicit none
   type :: derived_t
      integer :: x
   endtype derived_t
   type :: polymorphic_type_t
      class(derived_t), allocatable :: x
   contains
      generic :: assignment(=) => assign_polymorphic_type
      generic :: operator(+) => add_polymorphic_type
      procedure, pass(lhs) :: assign_polymorphic_type
      procedure, pass(lhs) :: add_polymorphic_type
   endtype polymorphic_type_t
contains
   subroutine assign_polymorphic_type(lhs, rhs)
      ! Operator `=`.
      class(polymorphic_type_t), intent(inout) :: lhs
      class(polymorphic_type_t), intent(in)    :: rhs
      if (allocated(rhs%x)) then
         if (.not.allocated(lhs%x)) allocate(derived_t :: lhs%x)
         select type(lhsx => lhs%x)
         type is(derived_t)
            select type(rhsx => rhs%x)
            type is(derived_t)
               lhsx%x = rhsx%x
            endselect
         endselect
      endif
   endsubroutine assign_polymorphic_type

   function add_polymorphic_type(lhs, rhs) result(res)
      ! Operator `+`.
      class(polymorphic_type_t), intent(in)  :: lhs
      class(polymorphic_type_t), intent(in)  :: rhs
      class(polymorphic_type_t), allocatable :: res
      allocate (polymorphic_type_t :: res)
      if (allocated(lhs%x).and.allocated(rhs%x)) then
         allocate(derived_t :: res%x)
         select type(lhsx => lhs%x)
         type is(derived_t)
            select type(rhsx => rhs%x)
            type is(derived_t)
               select type(resx => res%x)
               type is(derived_t)
                  resx%x = lhsx%x + rhsx%x
               endselect
            endselect
         endselect
      endif
   endfunction add_polymorphic_type
endmodule polymorphic_type_m

program leaks_raiser
   use polymorphic_type_m
   implicit none
   type(polymorphic_type_t) :: a
   type(polymorphic_type_t) :: b

   allocate(derived_t :: a%x)
   select type(ax => a%x)
   type is(derived_t)
      ax%x = 1
   endselect
   b = a
   b = a + b ! here the `+` operator could generate memory leaks
endprogram leaks_raiser
