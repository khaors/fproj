module test_pj
    use iso_fortran_env, only: wp=>real64,int64;
    use,intrinsic :: iso_c_binding, only: c_null_char;
    use testdrive, only: new_unittest, unittest_type, error_type,check;
    use fproj, only: pj_info,proj_info,print_info;
    implicit none;
    !
    private;
    !
    type(pj_info) :: pj_info_c;
    !
    public :: collect_pj;
    !
    contains
        subroutine collect_pj(testsuite)
            type(unittest_type),allocatable,intent(out) :: testsuite(:)
            !
            testsuite=[new_unittest("version_info",test_pj_info)];
            !
        end subroutine collect_pj
        !
        subroutine test_pj_info(error)
            type(error_type),allocatable,intent(out) :: error
            !
            pj_info_c=proj_info();
            call print_info(pj_info_c);
            !
        end subroutine test_pj_info
        !
end module test_pj


program tester
    use iso_fortran_env, only: error_unit, wp => real64;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_pj, only: collect_pj;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("proj", collect_pj)];
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
!
end program tester