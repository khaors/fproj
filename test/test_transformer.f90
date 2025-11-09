module test_transformer
    use stdlib_kinds, only: wp => dp;
    use stdlib_string_type, only: string_type,len;
    use stdlib_logger, only: global_logger,read_only_error;
    use transformer_utilities, only: transformer;
    use testdrive, only: new_unittest, unittest_type, error_type,check;
    !
    implicit none;
    !
    type(transformer) :: transf1
    !
    public :: collect_transformer
    !
    contains
!========================================================================================
        subroutine collect_transformer(testsuite)
!========================================================================================
            type(unittest_type),allocatable,intent(out) :: testsuite(:)
    !
            testsuite=[new_unittest("test_create_transformer",&
                test_create_transformer),&
                new_unittest("test_transform_transformer",&
                test_transform_transformer),&
                new_unittest("test_destroy_transformer",&
                test_destroy_transformer)];
    !
        end subroutine collect_transformer
!========================================================================================
        subroutine test_create_transformer(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            call transf1%create(4326,3116);
!
        end subroutine test_create_transformer
!========================================================================================
        subroutine test_transform_transformer(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            real(kind=wp),allocatable :: coords(:,:)!,y(:)
            integer :: i
!
            allocate(coords(4,2));
            coords(:,2)=(/-72.9311_wp,-73.11111_wp,-72.7833_wp,-72.9456_wp/);
            coords(:,1)=(/5.7161_wp,5.6161_wp,5.8161_wp,5.9161_wp/);
            do i=1,size(coords,1);
                write(*,*) i,coords(i,1:2);
            enddo
            call transf1%transform(coords,display=.true.);
            deallocate(coords);
!
        end subroutine test_transform_transformer
!========================================================================================
        subroutine test_destroy_transformer(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            call transf1%destroy();
!
        end subroutine test_destroy_transformer
!
end module test_transformer
!
program tester
    use iso_fortran_env, only: error_unit;
    use stdlib_kinds, only: wp => dp;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_transformer, only: collect_transformer;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("transformer", collect_transformer)];
    !
    do is=1,size(testsuites);
        write(error_unit,fmt) "Testing: ", testsuites(is)%name;
        call run_testsuite(testsuites(is)%collect, error_unit, stat);
    end do
    !
    if(stat > 0) then
        write(error_unit, '(i0, 1X, a)') stat, "test(s) failed";
        error stop
    end if

end program tester
