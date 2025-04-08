module test_pj_trans
    use iso_fortran_env, only: wp=>real64,int64;
    use,intrinsic :: iso_c_binding, only: c_null_char,c_ptr;
    use testdrive, only: new_unittest, unittest_type, error_type,check;
    use fproj_utilities, only: pj,pj_context_default,pj_coord,pj_area,&
        proj_create_crs_to_crs,proj_destroy,proj_trans,pj_fwd,pj_inv;
    implicit none;
    !
    private;
    !
    integer(kind=int64),parameter :: numchar=512;
    real(kind=wp),parameter :: tolerance=1.0d-10;
    type(pj) :: pj_obj1,pj_obj2;
    type(pj_area) :: area;
    type(pj_coord) :: coords,coords1,coords2;
    character(len=NUMCHAR) :: crs_source,crs_target;
    !
    public :: collect_pj_trans;
    !
    contains
        subroutine collect_pj_trans(testsuite)
            type(unittest_type),allocatable,intent(out) :: testsuite(:)
            !
            testsuite=[new_unittest("transformation1",test_transformation1),&
                new_unittest("transformation2",test_transformation2)];
            !
        end subroutine collect_pj_trans
        !
        subroutine test_transformation1(error)
            type(error_type),allocatable,intent(out) :: error
            !
            crs_source='+proj=longlat '//&
                '+datum=WGS84 '//&
                '+no_defs '//&
                '+type=crs';!'EPSG:4326';
            !
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
            pj_obj1 = proj_create_crs_to_crs(pj_context_default, &
                trim(crs_source)//c_null_char, &
                trim(crs_target)//c_null_char, area);
            !
            coords%x=-72.9311_wp;
            coords%y=5.7161_wp;
            !
            coords1=proj_trans(pj_obj1, pj_fwd, coords);
            write(*,*) 'Coords transf= ',coords1%x,coords1%y;
            !
            coords2=proj_trans(pj_obj1, pj_inv, coords1);
            write(*,*) 'Coords back transf= ',coords2%x,coords2%y;
            !
            call check(error, &
                dabs(coords%x-coords2%x) < tolerance .and. &
                dabs(coords%y-coords2%y) < tolerance);
            if(allocated(error)) return;
            !
            pj_obj1=proj_destroy(pj_obj1);
            !
        end subroutine test_transformation1
        !
        subroutine test_transformation2(error)
            type(error_type),allocatable,intent(out) :: error
            !
            crs_source='+proj=tmerc '//&
                '+lat_0=4.59620041666667 '//& 
                '+lon_0=-74.0775079166667 '//&
                '+k=1 +x_0=1000000 +y_0=1000000 '//&
                '+ellps=GRS80 '//&
                '+towgs84=0,0,0,0,0,0,0 '//& 
                '+units=m '//&
                '+no_defs '//&
                '+type=crs';!'EPSG:3116';
            crs_target='+proj=tmerc '//&
                '+lat_0=4 '//&
                '+lon_0=-73 '//& 
                '+k=0.9992 '//&
                '+x_0=5000000 '//& 
                '+y_0=2000000 '//& 
                '+ellps=GRS80 '//&
                '+towgs84=0,0,0,0,0,0,0 '//& 
                '+units=m +no_defs +type=crs';
            !
            pj_obj1 = proj_create_crs_to_crs(pj_context_default, &
                trim(crs_source)//c_null_char, &
                trim(crs_target)//c_null_char, area);
            !
            coords%x=1126995.5612103844_wp;
            coords%y=1123968.7220441916_wp;
            !
            coords1=proj_trans(pj_obj1, pj_fwd, coords);
                write(*,*) 'Coords transf= ',coords1%x,coords1%y;
            !
            pj_obj1=proj_destroy(pj_obj1);
                      

        end subroutine test_transformation2
end module test_pj_trans

program tester
    use iso_fortran_env, only: error_unit, wp => real64;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_pj_trans, only: collect_pj_trans;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("pj_trans", collect_pj_trans)];
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