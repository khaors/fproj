!! author: Oscar Garcia-Cabrejo
!! date: 11/08/2025
!! version: 0.1
!!  Define a class to represent the CRS (Coordinate Reference System)
!!
module CRS_utilities
!! The CRS class is used to encapsulate the information that defines a 
!! Coordinate Reference System
    use iso_fortran_env, only: wp => real64;
    use,intrinsic :: iso_c_binding;
    use fproj_utilities, only: pj,pj_context,proj_context_create,proj_context_destroy,&
        proj_create,proj_destroy,pj_coord,pj_fwd,pj_inv,pj_area,pj_context_default,&
        proj_create_crs_to_crs,proj_trans,proj_associated,proj_pj_info,print_info,&
        proj_create_crs_to_crs_from_pj,pj_info,proj_info,&
        proj_trans_f,proj_trans_f1,proj_trans_array,proj_errno,proj_is_deprecated,&
        proj_as_wkt,pj_wkt2_2019,strtofchar,proj_as_proj_string,proj_as_projjson,NUMCHAR
    use stdlib_string_type, only: string_type,len;
    use stdlib_strings, only: starts_with;
    use stdlib_error, only: check,error_stop;
    !
    type CRS
!! This class encapsulates the information used to define a Coordinate
!! Reference System CRS
        private
            character(len=NUMCHAR) :: epsg_string 
            integer :: epsg_code
            character(len=NUMCHAR) :: proj_string
            type(pj) :: pj_obj
        contains
            procedure,public :: create_from_epsg_number
            procedure,public :: create_from_epsg_string
            procedure,public :: create_from_proj4
            procedure,public :: destroy => destroy_crs
            !generic,operator (==) => crs_equal
            procedure,public :: write => write_crs

    end type CRS
!
    contains
!========================================================================================
        subroutine create_from_epsg_number(my_crs,epsg_code)
!========================================================================================
!! Constructor of the CRS class form the EPSG code
            class(CRS) :: my_crs
!! A `CRS` object
            integer :: epsg_code 
!! An integer variable with the EPSG code
            character(len=8) :: current_epsg_number
! Check if epsg_code starts with "EPSG:"
            my_crs%epsg_code=epsg_code;
            write(current_epsg_number, '(I8)') epsg_code;
            my_crs%epsg_string='EPSG:'//trim(current_epsg_number);
            write(*,*) 'EPSG String= ',trim(my_crs%epsg_string);
            my_crs%pj_obj=proj_create(pj_context_default,trim(my_crs%epsg_string)//&
                c_null_char);

            !
        end subroutine create_from_epsg_number
!========================================================================================
        subroutine create_from_epsg_string(my_crs,epsg_string)
!========================================================================================
!! Constructor of the CRS class form the EPSG string
            class(CRS) :: my_crs
!! A `CRS` object
            character(len=*) :: epsg_string
!! A character variable with the EPSG string
            if(starts_with(trim(epsg_string),"EPSG:")) then
                my_crs%epsg_string=trim(epsg_string);
                my_crs%pj_obj=proj_create(pj_context_default,trim(my_crs%epsg_string)//&
                    c_null_char);
            else
                call error_stop(msg="The EPSG code does not start with EPSG:");
            endif
        ! 
        end subroutine create_from_epsg_string
!========================================================================================
        subroutine create_from_proj4(my_crs,proj_string)
!========================================================================================
!! Constructor of the CRS class form the proj4 string
            class(CRS) :: my_crs
!! A `CRS` object
            character(len=*) :: proj_string
!! A character variable with the proj4 string
            my_crs%proj_string=trim(proj_string);
            my_crs%pj_obj=proj_create(pj_context_default,trim(my_crs%proj_string)//&
                c_null_char);
!
        end subroutine create_from_proj4
!========================================================================================
        subroutine destroy_crs(my_crs)
!========================================================================================
!! Destructor of the CRS class
            class(CRS) :: my_crs
!! A `CRS` object
            my_crs%pj_obj=proj_destroy(my_crs%pj_obj);
!
        end subroutine destroy_crs
!
!========================================================================================
        subroutine write_crs(my_crs,unit_,fmt)
!========================================================================================
!! Subroutine to write the information of a CRS object
            class(CRS) :: my_crs
!! A `CRS` object                
            integer :: unit_
!! An integer variable with the output unit
            character(len=*) :: fmt 
!! A character variable with the format to print the information 
!! of the CRS object
            call print_info(proj_pj_info(my_crs%pj_obj));
!
        end subroutine write_crs
!
end module CRS_utilities