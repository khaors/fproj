module test_fproj
    use,intrinsic :: iso_c_binding;
    use stdlib_kinds, only: wp => dp;
    use stdlib_string_type, only: string_type,len;
    use stdlib_logger, only: global_logger,read_only_error;
    use fproj_utilities, only: pj,pj_context,proj_context_create,proj_context_destroy,&
        proj_create,proj_destroy,pj_coord,pj_fwd,pj_inv,pj_area,pj_context_default,&
        proj_create_crs_to_crs,proj_trans,proj_associated,proj_pj_info,print_info,&
        proj_create_crs_to_crs_from_pj,pj_info,proj_info,&
        proj_trans_f,proj_trans_f1,proj_trans_array,proj_errno,proj_is_deprecated,&
        proj_as_wkt,pj_wkt2_2019,strtofchar,proj_as_proj_string,proj_as_projjson,&
        proj_is_crs
    use testdrive, only: new_unittest, unittest_type, error_type,check;
    !
    implicit none
    !
    real(kind=wp),parameter :: tolerance=1.0e-8;
    integer,parameter :: NUMCHAR=512;
    type(pj_context) :: pj_context_obj
    type(pj_area) :: area
    type(pj_info) :: pj_info_c
    type(pj) :: pj_obj,pj_obj_f,pj_obj_t,pj_obj_t3
    character(len=NUMCHAR) :: proj_from,proj_to
    type(c_ptr) :: current_wkt,options
    type(pj_coord) :: coords,coords1,coords2,vcoord(4), vcoord1(4), vcoord2(4),&
        original(4);
    integer :: n,res,i
    !
    public :: collect_fproj
    !
    contains
!========================================================================================
        subroutine collect_fproj(testsuite)
!========================================================================================
            type(unittest_type),allocatable,intent(out) :: testsuite(:)
!
            testsuite=[new_unittest("test_create_proj1", test_create_proj1),&
                new_unittest("test_wkt_proj1", test_wkt_proj1),&
                new_unittest("test_proj1_transform", test_proj1_transform),&
                new_unittest("test_proj1_destroy", test_proj1_destroy),&
                new_unittest("test_create_proj2", test_create_proj2),&
                new_unittest("test_proj2_transform", test_proj2_transform),&        
                new_unittest("test_proj2_destroy", test_proj2_destroy),&
                new_unittest("test_proj3_create", test_proj3_create),&
                new_unittest("test_proj3_transform", test_proj3_transform),&                
                new_unittest("test_proj3_destroy", test_proj3_destroy)];
!
        end subroutine collect_fproj
!========================================================================================
        subroutine test_create_proj1(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
        !
        proj_from='+proj=longlat '//&
                '+datum=WGS84 '//&
                '+no_defs '//&
                '+type=crs';!'EPSG:4326';
        proj_to='+proj=tmerc '//&
                '+lat_0=4.59620041666667 '//& 
                '+lon_0=-74.0775079166667 '//&
                '+k=1 +x_0=1000000 +y_0=1000000 '//&
                '+ellps=GRS80 '//&
                '+towgs84=0,0,0,0,0,0,0 '//& 
                '+units=m '//&
                '+no_defs '//&
                '+type=crs';!'EPSG:3115';
        pj_obj = proj_create_crs_to_crs(pj_context_default, trim(proj_from)//c_null_char, &
            trim(proj_to)//c_null_char, area);
        write(*,*) 'deprecated= ',proj_is_deprecated(pj_obj);
        call print_info(proj_pj_info(pj_obj));
        options=c_null_ptr;
        current_wkt=proj_as_wkt(pj_context_default,pj_obj,pj_wkt2_2019,options);
        write(*,*) 'wkt= ',trim(strtofchar(current_wkt,NUMCHAR));
!
        end subroutine test_create_proj1
!========================================================================================
        subroutine test_wkt_proj1(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            options=c_null_ptr;
            current_wkt=proj_as_wkt(pj_context_default,pj_obj,pj_wkt2_2019,options);
            write(*,*) 'wkt= ',trim(strtofchar(current_wkt,NUMCHAR));
!
            current_wkt=c_null_ptr;
            current_wkt=proj_as_projjson(pj_context_default,pj_obj,options);
            write(*,*) 'wkt= ',trim(strtofchar(current_wkt,NUMCHAR)); 
!
        end subroutine test_wkt_proj1
!========================================================================================
        subroutine test_proj1_transform(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            coords%x=-72.9311_wp;
            coords%y=5.7161_wp;
            write(*,*) 'original coords= ',coords%x,coords%y,pj_fwd;
            coords1=proj_trans(pj_obj, pj_fwd, coords);
            write(*,*) 'coords1= ',coords1%x,coords1%y;
            !
            coords2=proj_trans(pj_obj, pj_inv, coords1);
            write(*,*) 'coords2= ',coords2%x,coords2%y;
            !call check(dabs(coords%x-coords2%x) < tolerance  .and. &
            !    dabs(coords%y-coords2%y) < tolerance, &
            !    msg = 'coordinates x or y different in backtransformation');
            call check(error,coords%x,coords2%x,thr=tolerance);
            if(allocated(error)) return;
            call check(error,coords%y,coords2%y,thr=tolerance);
            if(allocated(error)) return;
!           
        end subroutine test_proj1_transform
!========================================================================================
        subroutine test_proj1_destroy(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            pj_obj=proj_destroy(pj_obj);
            call check(error, .not. proj_associated(pj_obj));
            if(allocated(error)) return;
!
        end subroutine test_proj1_destroy
!========================================================================================
        subroutine test_create_proj2(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
        !
        proj_to='EPSG:9377';
        pj_obj_t=proj_create(pj_context_default,trim(proj_to)//c_null_char);
        call check(error, proj_associated(pj_obj_t));
        if(allocated(error)) return;
        write(*,*) 'proj_is_crs= ',proj_is_crs(pj_obj_t);
        call print_info(proj_pj_info(pj_obj_t));
!
        end subroutine test_create_proj2
!========================================================================================
        subroutine test_proj2_transform(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            vcoord(:)%x=(/-72.9311_wp,-73.11111_wp,-72.7833_wp,-72.9456_wp/);
            vcoord(:)%y=(/5.7161_wp,5.6161_wp,5.8161_wp,5.9161_wp/);
            write(*,*) 'original= ',vcoord(:)%x;
            write(*,*) 'original= ',vcoord(:)%y;
            n=4_c_size_t;
            pj_obj = proj_create_crs_to_crs(pj_context_default, trim(proj_from)//c_null_char, &
            trim(proj_to)//c_null_char, area);
            write(*,*) 'deprecated= ',proj_is_deprecated(pj_obj);
            call print_info(proj_pj_info(pj_obj));

            res = proj_trans_f(pj_obj, pj_fwd, vcoord);
            call check(error, res == 0);
            if(allocated(error)) return;
            vcoord1=vcoord;
            write(*,*) 'Transform';
            do i=1,4
                write(*,*) 'vcoord1= ',vcoord1(i)%x,vcoord1(i)%y;
            end do
!
        end subroutine test_proj2_transform
!========================================================================================
        subroutine test_proj2_destroy(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            pj_obj_t=proj_destroy(pj_obj_t);
            call check(error, .not. proj_associated(pj_obj_t));
            if(allocated(error)) return;
            pj_obj=proj_destroy(pj_obj);
!
            write(*,*) 'proj_errorno= ', proj_errno(pj_obj);

!
        end subroutine test_proj2_destroy
!========================================================================================
        subroutine test_proj3_create(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            proj_from="EPSG:4326";
            proj_to="EPSG:3116";
            pj_obj_t3=proj_create_crs_to_crs(pj_context_default, trim(proj_from)//c_null_char, &
            trim(proj_to)//c_null_char, area);
            write(*,*) 'deprecated= ',proj_is_deprecated(pj_obj_t3);
            call print_info(proj_pj_info(pj_obj_t3));
!
        end subroutine test_proj3_create
!========================================================================================
        subroutine test_proj3_transform(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
! Lat and Lot area assigned to the x and y components of the vcoord structure
!
            vcoord(:)%y=(/-72.9311_wp,-73.11111_wp,-72.7833_wp,-72.9456_wp/);
            vcoord(:)%x=(/5.7161_wp,5.6161_wp,5.8161_wp,5.9161_wp/);
!
            original=vcoord;
!
            do i=1,4
                write(*,*) 'original= ',vcoord(i)%x,vcoord(i)%y;
            enddo
!
            res = proj_trans_f(pj_obj_t3, pj_fwd, vcoord);
            call check(error, res == 0);
            if(allocated(error)) return;
            vcoord1=vcoord;
            write(*,*) 'Transform';
            do i=1,4
                write(*,*) 'vcoord1= ',vcoord1(i)%x,vcoord1(i)%y;
            end do
!
            res = proj_trans_f(pj_obj_t3, pj_inv, vcoord1);
            vcoord2=vcoord1;
            write(*,*) 'Back Transform';
            do i=1,4
                write(*,*) 'vcoord2= ',vcoord2(i)%x,vcoord2(i)%y;
            end do
!
            do i=1,4
                call check(error,original(i)%x,vcoord2(i)%x,thr=tolerance);
                if(allocated(error)) return;
                call check(error,original(i)%y,vcoord2(i)%y,thr=tolerance);
                if(allocated(error)) return;
            enddo 
!
        end subroutine test_proj3_transform           
!========================================================================================
        subroutine test_proj3_destroy(error)
!========================================================================================
            type(error_type),allocatable,intent(out) :: error
!
            pj_obj_t3=proj_destroy(pj_obj_t3);
            call check(error, .not. proj_associated(pj_obj_t3));
            if(allocated(error)) return;
!
        end subroutine test_proj3_destroy
!
end module test_fproj
!
program tester
!
    use iso_fortran_env, only: error_unit;
    use stdlib_kinds, only: wp => dp;
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_fproj, only: collect_fproj;
    implicit none;
    type(testsuite_type),allocatable :: testsuites(:);
    character(len=*),parameter :: fmt = '("#", *(1x, a))';
    integer :: stat,is
    !
    stat=0;
    !
    testsuites=[new_testsuite("fproj", collect_fproj)];
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


