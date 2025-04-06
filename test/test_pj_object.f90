module test_pj_object
    use iso_fortran_env, only: wp=>real64,int64;
    use,intrinsic :: iso_c_binding, only: c_null_char,c_ptr;
    use testdrive, only: new_unittest, unittest_type, error_type,check;
    use fproj, only: pj,proj_create,proj_destroy,pj_context,&
        pj_context_default,pj_area,proj_create_crs_to_crs,&
        proj_associated,proj_is_crs,proj_context_create,&
        proj_create_crs_to_crs_from_pj,pj_comp_strict,proj_is_equivalent_to;
    implicit none;
    !
    private;
    !
    integer(kind=int64),parameter :: numchar=521;
    type(pj) :: pj_obj,pj_obj1,pj_obj2
    type(pj_context) :: pj_context_obj
    type(pj_area) :: area
    character(len=numchar) :: crs_source,crs_target
    type(c_ptr) :: options
    !
    public :: collect_pj_objects
    !
contains
    subroutine collect_pj_objects(testsuite)
        type(unittest_type),allocatable,intent(out) :: testsuite(:)
        !
        testsuite=[new_unittest("create",test_create_pj),&
            new_unittest("create_from_crs",test_create_pj_from_crs),&
            new_unittest("create_from_crs_from_pj",test_create_crs_to_crs_from_pj),&
            new_unittest("pj_is_crs",test_pj_is_crs)]

    end subroutine collect_pj_objects
    !
    subroutine test_create_pj(error)
        type(error_type),allocatable,intent(out) :: error
        !
        crs_source='+proj=longlat '//&
            '+datum=WGS84 '//&
            '+no_defs '//&
            '+type=crs';!'EPSG:4326';
        pj_context_obj=proj_context_create();
        pj_obj1=proj_create(pj_context_obj,crs_source);
        call check(error,proj_associated(pj_obj1),.true.);
        if(allocated(error)) return;
        pj_obj1=proj_destroy(pj_obj1);
        !
    end subroutine test_create_pj
    !
    subroutine test_create_pj_from_crs(error)
        type(error_type),allocatable,intent(out) :: error
        !
        crs_source='+proj=longlat '//&
            '+datum=WGS84 '//&
            '+no_defs '//&
            '+type=crs';!'EPSG:4326';
         crs_target='+proj=tmerc '//&
              '+lat_0=4.59620041666667 '//& 
              '+lon_0=-74.0775079166667 '//&
              '+k=1 +x_0=1000000 +y_0=1000000 '//&
              '+ellps=GRS80 '//&
              '+towgs84=0,0,0,0,0,0,0 '//& 
              '+units=m '//&
              '+no_defs '//&
              '+type=crs';!'EPSG:3116';
         pj_obj = proj_create_crs_to_crs(pj_context_default, &
             trim(crs_source)//c_null_char, &
             trim(crs_target)//c_null_char, area);
         call check(error,proj_associated(pj_obj),.true.);
         if(allocated(error)) return;
         pj_obj=proj_destroy(pj_obj);
 
    end subroutine test_create_pj_from_crs
    !
    subroutine test_create_crs_to_crs_from_pj(error)
        type(error_type),allocatable,intent(out) :: error
        !
        crs_source='+proj=longlat '//&
            '+datum=WGS84 '//&
            '+no_defs '//&
            '+type=crs';!'EPSG:4326';
        crs_target='+proj=tmerc '//&
              '+lat_0=4.59620041666667 '//& 
              '+lon_0=-74.0775079166667 '//&
              '+k=1 +x_0=1000000 +y_0=1000000 '//&
              '+ellps=GRS80 '//&
              '+towgs84=0,0,0,0,0,0,0 '//& 
              '+units=m '//&
              '+no_defs '//&
              '+type=crs';!'EPSG:3116';
        !
        pj_obj=proj_create(pj_context_default,trim(crs_source)//c_null_char);
        !
        pj_obj1=proj_create(pj_context_default,trim(crs_target)//c_null_char);
        !
        pj_obj2=proj_create_crs_to_crs_from_pj(pj_context_default,pj_obj1,&
            pj_obj1,area,options);
        !
        call check(error,proj_associated(pj_obj2),.true.);
        if(allocated(error)) return;
        !
        write(*,*) 'compare= ',proj_is_equivalent_to(pj_obj,pj_obj1,pj_comp_strict)
        !
        pj_obj2=proj_destroy(pj_obj2);
        pj_obj1=proj_destroy(pj_obj1);
        pj_obj=proj_destroy(pj_obj);
        !
    end subroutine test_create_crs_to_crs_from_pj
    !
    subroutine test_pj_is_crs(error)
        type(error_type),allocatable,intent(out) :: error
        !
        write(*,*) 'crs= ',proj_is_crs(pj_obj);
        call check(error,proj_is_crs(pj_obj),0);
        !
    end subroutine test_pj_is_crs

end module test_pj_object

program tester
    use iso_fortran_env, only: error_unit, wp => real64;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_pj_object, only: collect_pj_objects;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("pj_objects", collect_pj_objects)];
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