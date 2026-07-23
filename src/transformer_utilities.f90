module transformer_utilities
    use iso_fortran_env, only: wp => real64;
    use :: iso_c_binding;
    use fproj_utilities, only: pj,pj_context,proj_context_create,proj_context_destroy,&
        proj_create,proj_destroy,pj_coord,pj_fwd,pj_inv,pj_area,pj_context_default,&
        proj_create_crs_to_crs,proj_trans,proj_associated,proj_pj_info,print_info,&
        proj_create_crs_to_crs_from_pj,pj_info,proj_info,&
        proj_trans_f,proj_trans_f1,proj_trans_array,proj_errno,proj_is_deprecated,&
        proj_as_wkt,pj_wkt2_2019,strtofchar,proj_as_proj_string,proj_as_projjson
    use CRS_utilities, only: CRS;
    use stdlib_strings, only: starts_with;
    use stdlib_error, only: check,error_stop;
!
    type transformer
        private
            type(CRS) :: from,to
            type(pj_context) :: pj_context_obj
            type(pj_area) :: area
            type(pj) :: pj_obj
            type(pj_coord),allocatable :: coord_values(:),original_values(:)
        contains
            procedure,public :: create_from_crs_numbers
            procedure,public :: create_from_crs_strings
            generic,public :: create => create_from_crs_numbers,&
                create_From_crs_strings
            procedure,public :: destroy => destroy_transformer
            procedure,public :: transform => transform_values
            procedure,public :: transform_inverse => transform_inverse_values
            procedure,public :: write => write_transform
    end type transformer
! !
contains
!========================================================================================
    subroutine create_from_crs_numbers(my_transformer,code_from,code_to)
!========================================================================================
        class(transformer) :: my_transformer
        integer :: code_from,code_to
!
        character(len=13) :: proj_from,proj_to
        integer :: number_digits_from,number_digits_to
        character(len=2) :: number_from,number_to
!
        write(*,*) 'Inside create_from_crs_numbers';
        if(code_from < 0) then
            call error_stop(msg="The EPSG code from is negative");
        endif
!
        if(code_to < 0) then
            call error_stop(msg="The EPSG code to is negative");
        endif
!
! Check if EPSG code from has 4 or 5 digits
!
        if(code_from > 1000 .and. code_from < 9999) then
            number_digits_from=4;
        else
            number_digits_from=5;
        endif
!
        if(code_to > 1000 .and. code_to < 9999) then
            number_digits_to=4;
        else
            number_digits_to=5;
        endif
!
        write(*,*) 'Number digits= ',number_digits_from,number_digits_to;
        write(number_from,'(I2)') number_digits_from;
        write(number_to,'(I2)') number_digits_to;
!        write(*,*) 'Number digits= ',number_from,number_to;

!
        write(proj_from, '(I'//trim(number_from)//')') code_from;
        write(proj_to, '(I'//trim(number_to)//')') code_to;
        proj_from="EPSG:"//trim(proj_from);
        proj_to="EPSG:"//trim(proj_to);
        write(*,*) 'proj_from= ',trim(proj_from);
        write(*,*) 'proj_to= ',trim(proj_to);
!
        my_transformer%pj_obj = proj_create_crs_to_crs(&
            pj_context_default, &
            trim(proj_from)//c_null_char, &
            trim(proj_to)//c_null_char,&
            my_transformer%area);
        !
        write(*,*) 'associated= ',proj_associated(my_transformer%pj_obj);
        call print_info(proj_pj_info(my_transformer%pj_obj));
!
    end subroutine create_from_crs_numbers
!========================================================================================
    subroutine create_from_crs_strings(my_transformer,string_from,string_to)
!========================================================================================
        class(transformer) :: my_transformer
        character(len=*) :: string_from,string_to
!
        if(starts_with(string_from,"EPSG:") .and. starts_with(string_to,"EPSG:")) then
            my_transformer%pj_obj = proj_create_crs_to_crs(&
                pj_context_default, &
                trim(string_from)//c_null_char, &
                trim(string_to)//c_null_char,&
                my_transformer%area);
        else
            call error_stop(msg="The EPSG codes do not start with EPSG:");
        endif
    end subroutine create_from_crs_strings
!========================================================================================
    subroutine destroy_transformer(my_transformer)
!========================================================================================
        class(transformer) :: my_transformer
!
        if(allocated(my_transformer%coord_values)) then
            deallocate(my_transformer%coord_values);
        endif
!
        if(allocated(my_transformer%original_values)) then
            deallocate(my_transformer%original_values);
        endif
!
        call my_transformer%from%destroy();
        call my_transformer%to%destroy();
!
        my_transformer%pj_obj=proj_destroy(my_transformer%pj_obj);
!
    end subroutine destroy_transformer
!========================================================================================
    subroutine transform_values(my_transformer,coord,display)
!========================================================================================
        class(transformer) :: my_transformer
        real(kind=wp),dimension(:,:) :: coord
        logical,optional :: display
!
        integer :: number_data,ierr,res,i
!
        if(allocated(my_transformer%coord_values)) then
            deallocate(my_transformer%coord_values);
        endif
        if(allocated(my_transformer%original_values)) then
            deallocate(my_transformer%original_values);
        endif
!
        number_data=size(coord,1);
        allocate(my_transformer%coord_values(number_data),stat=ierr);
        call check(ierr == 0, msg = 'Error while allocating memory for coord_values array');
        my_transformer%coord_values(:)%x=coord(:,1);
        my_transformer%coord_values(:)%y=coord(:,2);
        my_transformer%original_values=my_transformer%coord_values;
!
        res = proj_trans_f(my_transformer%pj_obj, &
            pj_fwd, my_transformer%coord_values);
        call check(res == 0, msg = 'Error during transformation');
!
        if(present(display)) then
            if(display) then
                call my_transformer%write(6,'(I4,1X,2f15.6)');
            endif
        endif
!
    end subroutine transform_values
!========================================================================================
    subroutine transform_inverse_values(my_transformer,coord,display)
!========================================================================================
        class(transformer) :: my_transformer
        real(kind=wp),dimension(:,:) :: coord
        logical,optional :: display
!
        integer :: res,i
!
        if(allocated(my_transformer%coord_values)) then
            deallocate(my_transformer%coord_values);
        endif
        if(allocated(my_transformer%original_values)) then
            deallocate(my_transformer%original_values);
        endif
!
        my_transformer%coord_values%x=coord(:,1);
        my_transformer%coord_values%y=coord(:,2);
        my_transformer%original_values=my_transformer%coord_values;
!
        res = proj_trans_f(my_transformer%pj_obj, &
            pj_inv, my_transformer%coord_values);
        call check(res == 0, msg = 'Error during inverse transformation');
        if(present(display)) then
            if(display) then
                call my_transformer%write(6,'($I,1X,2f15.6)');
            endif
        endif
!
    end subroutine transform_inverse_values
!========================================================================================
    subroutine write_transform(my_transformer,unit_,format_)
!========================================================================================
        class(transformer) :: my_transformer
        integer :: unit_
        character(len=*) :: format_
!
        integer :: i
!
        if(allocated(my_transformer%coord_values)) then
            do i=1,size(my_transformer%coord_values);
                write(unit_,format_) i,my_transformer%coord_values(i)%x,&
                    my_transformer%coord_values(i)%y;
            enddo
        endif
!
    end subroutine write_transform
!
end module transformer_utilities
