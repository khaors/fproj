module test_transformer
    use stdlib_kinds, only: wp => dp;
    use stdlib_string_type, only: string_type,len;
    use stdlib_logger, only: global_logger,read_only_error;
    use transformer_utilities, only: transformer;
    use testdrive, only: new_unittest, unittest_type, error_type,check;
    !
    implicit none;
    !
    type(transformer) :: transf1,transf2
    !
    public :: collect_transformer
    !
    contains
!========================================================================================
        subroutine collect_transformer(testsuite)
!========================================================================================
            type(unittest_type),allocatable,intent(out) :: testsuite(:)
    !
            testsuite=[new_unittest("test_create_transformer1",&
                test_create_transformer1),&
                new_unittest("test_transform_transformer1",&
                test_transform_transformer1),&
                new_unittest("test_destroy_transformer1",&
                test_destroy_transformer1),&
                new_unittest("test_create_transformer2",&
                test_create_transformer2),&
                new_unittest("test_transform_transformer2",&
                test_transform_transformer2),&
                new_unittest("test_destroy_transformer2",&
                test_destroy_transformer2)];
    !
        end subroutine collect_transformer
!========================================================================================
        subroutine test_create_transformer1(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            call transf1%create(4326,3116);
!
        end subroutine test_create_transformer1
!========================================================================================
        subroutine test_transform_transformer1(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            real(kind=wp),allocatable :: coords(:,:)!,y(:)
            integer :: i
!
            allocate(coords(4,2));
            ! Lat and long are inverted
            coords(:,2)=(/-72.9311_wp,-73.11111_wp,-72.7833_wp,-72.9456_wp/);
            coords(:,1)=(/5.7161_wp,5.6161_wp,5.8161_wp,5.9161_wp/);
            do i=1,size(coords,1);
                write(*,*) i,coords(i,1:2);
            enddo
            call transf1%transform(coords,display=.true.);
            deallocate(coords);
!
        end subroutine test_transform_transformer1
!========================================================================================
        subroutine test_destroy_transformer1(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            call transf1%destroy();
!
        end subroutine test_destroy_transformer1
!
!========================================================================================
        subroutine test_create_transformer2(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            call transf2%create(4326,32633);
!
        end subroutine test_create_transformer2
!========================================================================================
        subroutine test_transform_transformer2(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            real(kind=wp),allocatable :: coords(:,:)!,y(:)
            integer :: i
!
            allocate(coords(4,2));
            ! Lat and long are inverted
            coords(:,2)=(/-12.49_wp,-12.34_wp,-12.51_wp,-12.56_wp/);
            coords(:,1)=(/41.89_wp,41.78_wp,41.76_wp,41.91_wp/);
            do i=1,size(coords,1);
                write(*,*) i,coords(i,1:2);
            enddo
            call transf2%transform(coords,display=.true.);
            deallocate(coords);
!
        end subroutine test_transform_transformer2
!========================================================================================
        subroutine test_destroy_transformer2(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            call transf2%destroy();
!
        end subroutine test_destroy_transformer2
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
