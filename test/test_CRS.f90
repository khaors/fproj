module test_crs
    use stdlib_kinds, only: wp => dp;
    use stdlib_string_type, only: string_type,len;
    use stdlib_logger, only: global_logger,read_only_error;
    use CRS_utilities, only: CRS;
    use testdrive, only: new_unittest, unittest_type, error_type,check;
    !
    implicit none;
    !
    type(CRS) :: crs_from,crs_to
    !
    public :: collect_crs
    !
    contains
!========================================================================================
        subroutine collect_crs(testsuite)
!========================================================================================
            type(unittest_type),allocatable,intent(out) :: testsuite(:)
!
            testsuite=[new_unittest("test_create_crs", test_create_crs),&
                new_unittest("test_write_crs", test_write_crs),&
                new_unittest("test_destroy_crs", test_destroy_crs)]

        end subroutine collect_crs
!========================================================================================
        subroutine test_create_crs(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
! '+proj=longlat +datum=WGS84 +no_defs +type=crs'
            call crs_from%create_from_epsg_number(4326);
            call crs_to%create_from_epsg_number(3116);
!
        end subroutine test_create_crs
!========================================================================================
        subroutine test_write_crs(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            call crs_from%write(6,'');
            call crs_to%write(6,'');
!
        end subroutine test_write_crs
!========================================================================================
        subroutine test_destroy_crs(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            call crs_from%destroy();
            !call crs_to%destroy();
!
        end subroutine test_destroy_crs
!
end module test_crs

program tester
    use iso_fortran_env, only: error_unit;
    use stdlib_kinds, only: wp => dp;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_crs, only: collect_crs;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("fproj", collect_crs)];
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
