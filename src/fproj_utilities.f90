module fproj_utilities
    use iso_fortran_env, only: wp => real64;
    use,intrinsic :: iso_c_binding, only: c_int,&
        c_double,&
        c_ptr,&
        c_null_ptr,&
        c_char,&
        c_size_t,&
        c_associated,&
        c_signed_char,&
        c_f_pointer,&
        c_null_char;
    implicit none;
    !
    integer(c_int),parameter :: NUMCHAR=512;
    ! pj_direction
    enum,bind(c) 
        enumerator :: &
        pj_fwd = 1_c_int, &
        pj_ident= 0_c_int, &
        pj_inv = -1_c_int
    end enum  
    ! pj_category
    enum,bind(c)
        enumerator :: pj_category_ellipsoid,&
        pj_category_prime_meridian,&
        pj_category_datum,&
        pj_category_crs,&
        pj_category_coordinate_operation,&
        pj_category_datum_ensemble
    end enum
  !pj_type
	enum, bind(c)
	  enumerator :: pj_type_unknown, &
	    pj_type_ellipsoid, &
	    pj_type_prime_meridian, &
	    pj_type_geodetic_reference_frame, &
        pj_type_dynamic_geodetic_reference_frame, &
	    pj_type_vertical_reference_frame, &
        pj_type_dynamic_vertical_reference_frame, &
	    pj_type_datum_ensemble, &
	    pj_type_crs, &
	    pj_type_geodetic_crs, &
        pj_type_geocentric_crs, &
	    pj_type_geographic_crs, &
        pj_type_geographic_2d_crs, &
        pj_type_geographic_3d_crs, &
	    pj_type_vertical_crs, &
        pj_type_projected_crs, &
        pj_type_compound_crs, &
	    pj_type_temporal_crs, &
        pj_type_engineering_crs, &
        pj_type_bound_crs, &
        pj_type_other_crs, &
	    pj_type_conversion, &
        pj_type_transformation, &
        pj_type_concatenated_operation, &
	    pj_type_other_coordinate_operation, &
	    pj_type_temporal_datum, &
        pj_type_engineering_datum, &
        pj_type_parametric_datum,&
        pj_type_derived_projected_crs, &
        pj_type_coordinate_metadata 
	end enum
    !pj_comparison
    enum,bind(c) 
        enumerator :: pj_comp_strict, &
            pj_comp_equivalent, &
            pj_comp_equivalent_except_axis_order_geogcrs
    end enum
    !pj_wkt
    enum,bind(c)
        enumerator ::  pj_wkt2_2015, &
            pj_wkt2_2015_simplified, &
            pj_wkt2_2019, &
            pj_wkt2_2018, &
            pj_wkt2_2019_simplified, &
            pj_wkt2_2018_simplified, &
            pj_wkt1_gdal, &
            pj_wkt1_esri  
    end enum
    !pj_crs
    enum,bind(c)
        enumerator :: pj_crs_extent_none,&
        !test coordinate operation extent against both crs extent.
        pj_crs_extent_both,&
        !test coordinate operation extent against the intersection of both crs extent.
        pj_crs_extent_intersection,&
        !test coordinate operation against the smallest of both crs extent.
        pj_crs_extent_smallest
    end enum
    !pj_proj
    enum,bind(c)
        enumerator :: pj_proj_5,pj_proj_4
    end enum
    !pj_cs_type
    enum,bind(c)
        enumerator :: pj_cs_type_unknown,&
        pj_cs_type_cartesian,&
        pj_cs_type_ellipsoidal,&
        pj_cs_type_vertical,&
        pj_cs_type_spherical,&
        pj_cs_type_ordinal,&
        pj_cs_type_parametric,&
        pj_cs_type_datetimetemporal,&
        pj_cs_type_temporalcount,&
        pj_cs_type_temporalmeasure
    end enum
    !pj_log
    enum,bind(c)
        enumerator :: pj_log_none = 0_c_int,&
                      pj_log_error = 1_c_int,&
                      pj_log_debug = 2_c_int,&
                      pj_log_trace = 3_c_int,&
                      pj_log_tell = 4_c_int,&
                      pj_log_debug_major = 2_c_int,& ! for proj_api.h compatibility
                      pj_log_debug_minor = 3_c_int  ! for proj_api.h compatibility
    end enum
    !
    type,bind(c) :: pj
        private
            type(c_ptr) :: ptr=c_null_ptr;
    end type pj
    !
    type(pj),parameter :: pj_null=pj(c_null_ptr);
    !
    type,bind(c) :: pj_context
        private
            type(c_ptr) :: ptr=c_null_ptr;
    end type pj_context
    !
    type(pj_context),parameter :: pj_context_default=pj_context(c_null_ptr);
    !
    type,bind(c) :: pj_area
        private
            type(c_ptr) :: ptr = c_null_ptr;
    end type pj_area
    ! 2D Coords
    ! Latitude and Longitude
    type,bind(c) :: pj_lp
        !private
        real(kind=c_double) :: lam=0.0_c_double;
        real(kind=c_double) :: phi=0.0_c_double;
    end type pj_lp
    ! Cartesian coordinates
    type,bind(c) :: pj_xy
        !private
        real(kind=c_double) :: x=0.0_c_double;
        real(kind=c_double) :: y=0.0_c_double;
    end type pj_xy
    ! Generic coordinates
    type,bind(c) :: pj_uv
        real(kind=c_double) :: u=0.0_c_double;
        real(kind=c_double) :: v=0.0_c_double;
    end type pj_uv
    ! 3D Coords
    ! Longitude, latitude, vertical component
    type,bind(c) :: pj_lpz
        real(kind=c_double) :: lam=0.0_c_double;
        real(kind=c_double) :: phi=0.0_c_double;
        real(kind=c_double) :: z=0.0_c_double;
    end type pj_lpz
    ! 3D Cartersian
    type,bind(c) :: pj_xyz
        real(kind=c_double) :: x=0.0_c_double;
        real(kind=c_double) :: y=0.0_c_double;
        real(kind=c_double) :: z=0.0_c_double;    
    end type pj_xyz
    ! 3D Generic coords
    type,bind(c) :: pj_uvw
        real(kind=c_double) :: u=0.0_c_double;
        real(kind=c_double) :: v=0.0_c_double;
        real(kind=c_double) :: w=0.0_c_double;    
    end type pj_uvw
    ! Spatio-temporal coordinates
    ! Longitude, latitude, z and time
    type,bind(c) :: pj_lpzt
        real(kind=c_double) :: lam=0.0_c_double;
        real(kind=c_double) :: phi=0.0_c_double;
        real(kind=c_double) :: z=0.0_c_double;
        real(kind=c_double) :: t=0.0_c_double;    
    end type pj_lpzt
    ! Euclidean spatio-temporal coordinates
    type,bind(c) :: pj_xyzt
        real(kind=c_double) :: x=0.0_c_double;
        real(kind=c_double) :: y=0.0_c_double;
        real(kind=c_double) :: z=0.0_c_double;
        real(kind=c_double) :: t=0.0_c_double;    
    end type pj_xyzt
    ! Generic spatio-temporal coordinates
    type,bind(c) :: pj_uvwt
        real(kind=c_double) :: u=0.0_c_double;
        real(kind=c_double) :: v=0.0_c_double;
        real(kind=c_double) :: w=0.0_c_double;
        real(kind=c_double) :: t=0.0_c_double;    
    end type pj_uvwt
    ! Ancillary types for geodetic computions
    type,bind(c) :: pj_opk
        real(kind=c_double) :: o=0.0_c_double;
        real(kind=c_double) :: p=0.0_c_double;
        real(kind=c_double) :: k=0.0_c_double;
    end type pj_opk
    !
    type,bind(c) :: pj_enu
        real(kind=c_double) :: e=0.0_c_double;
        real(kind=c_double) :: n=0.0_c_double;
        real(kind=c_double) :: u=0.0_c_double;        
    end type pj_enu
    !
    type,bind(c) :: pj_geod
        real(kind=c_double) :: s=0.0_c_double;
        real(kind=c_double) :: a1=0.0_c_double;
        real(kind=c_double) :: a2=0.0_c_double;        
    end type pj_geod
    ! Complex coordinate types
    type,bind(c) :: pj_coord
        real(kind=c_double) :: x=huge(1.0_c_double);
        real(kind=c_double) :: y=huge(1.0_c_double);
        real(kind=c_double) :: z=0.0_c_double;
        real(kind=c_double) :: t=0.0_c_double;
    end type pj_coord
    ! Projection derivatives
    type,bind(c) :: pj_factors
        real(kind=c_double) :: meridional_scale; !h
        real(kind=c_double) ::parallel_scale;    !k
        real(kind=c_double) :: areal_scale;      !s

	    real(kind=c_double) :: angular_distortion;      !omega
	    real(kind=c_double) :: meridian_parallel_angle; !theta-prime
	    real(kind=c_double) :: meridian_convergence;    !alpha

	    real(kind=c_double) :: tissot_semimajor;  !a
	    real(kind=c_double) :: tissot_semiminor;  !b

	    real(kind=c_double) :: dx_dlam;
	    real(kind=c_double) :: dx_dphi;
	    real(kind=c_double) :: dy_dlam;
	    real(kind=c_double) :: dy_dphi;
    end type pj_factors
    !
    type,bind(c) :: pj_ellps
         character(kind=c_char) :: id(NUMCHAR)
         character(kind=c_char) :: major(NUMCHAR)
         character(kind=c_char) :: ell(NUMCHAR)
         character(kind=c_char) :: name(NUMCHAR)
    end type pj_ellps
    !
    type,bind(c) :: pj_units
        character(kind=c_char) :: id(NUMCHAR)
        character(kind=c_char) :: to_meter(NUMCHAR)
        character(kind=c_char) :: name(NUMCHAR)
        character(kind=c_char) :: factor(NUMCHAR)
    end type pj_units
    !
    type,bind(c) :: pj_prime_meridians
        character(kind=c_char) :: id(NUMCHAR)
        character(kind=c_char) :: defn(NUMCHAR)
    end type pj_prime_meridians
    ! Info structure
    type,bind(c) :: pj_info
        integer(kind=c_int) :: major 
        integer(kind=c_int) :: minor 
        integer(kind=c_int) :: patch
        type(c_ptr) :: release
        type(c_ptr) :: version
        type(c_ptr) :: searchpath
        type(c_ptr) :: paths
        integer(kind=c_size_t) :: path_count
    end type pj_info
    !
    type,bind(c) :: pj_proj_info
        type(c_ptr) :: id
        type(c_ptr) :: description
        type(c_ptr) :: definition
        integer(kind=c_int) :: has_inverse=-1_c_int;
        real(kind=c_double) :: accuracy=-1.0_c_double;
    end type pj_proj_info
    !
    type,bind(c) :: pj_grid_info
        type(c_ptr) :: gridname
        type(c_ptr) :: filename
        type(c_ptr) :: format
        type(pj_lp) :: lowerleft
        type(pj_lp) :: upperright
        integer(kind=c_int) :: n_lon,n_lat
        real(kind=c_double) :: cs_lon,cs_lat 
    end type pj_grid_info
    !
    type,bind(c) :: pj_crs_info
        type(c_ptr) :: auth_name
        type(c_ptr) :: code
        type(c_ptr) :: name 
        integer(kind=kind(pj_type_unknown)) :: type
        integer(kind=c_int) :: deprecated
        integer(kind=c_int) :: boox_valid
        real(kind=c_double) :: west_lon_degree
        real(kind=c_double) :: south_lat_degree
        real(kind=c_double) :: east_lon_degree
        real(kind=c_double) :: north_lat_degree
        type(c_ptr) :: area_name
        type(c_ptr) :: projection_method_name
        type(c_ptr) :: celestial_body_name 
    end type pj_crs_info
    !
    type,bind(c) :: pj_string_list
        type(c_ptr) :: ptr(NUMCHAR)
    end type pj_string_list
    !
    type,bind(c) :: pj_init_info
        type(c_ptr) :: name
        type(c_ptr) :: filename
        type(c_ptr) :: version
        type(c_ptr) :: origin
        type(c_ptr) :: lastupdate
    end type pj_init_info
    !
    ! PROJ functions
    !
	interface
	    function proj_create(ctx, definition) bind(c,name='proj_create')
            import
            type(pj_context),value :: ctx
            character(kind=c_char) :: definition(*)
            type(pj) :: proj_create
            end function proj_create
        end interface
	!
	interface
        function proj_create_crs_to_crs(ctx, source_crs, target_crs, area) &
        bind(c,name='proj_create_crs_to_crs')
            import
            type(pj_context),value :: ctx
            character(kind=c_char) :: source_crs(*)
            character(kind=c_char) :: target_crs(*)
            type(pj_area),value :: area
            type(pj) :: proj_create_crs_to_crs
        end function proj_create_crs_to_crs
	end interface

	interface
        function proj_create_crs_to_crs_from_pj(ctx, source_crs, target_crs, &
            area, options) bind(c,name='proj_create_crs_to_crs_from_pj')
            import
            type(pj_context),value :: ctx
            type(pj),value :: source_crs
            type(pj),value :: target_crs
            type(pj_area),value :: area
            type(c_ptr),value :: options
            type(pj) :: proj_create_crs_to_crs_from_pj
        end function proj_create_crs_to_crs_from_pj
	end interface
	!
	interface
        function proj_destroy(p) bind(c,name='proj_destroy')
            import
            type(pj),value :: p
            type(pj) :: proj_destroy
        end function proj_destroy
	end interface
    !
    interface
        function proj_is_deprecated(p) bind(c,name='proj_is_deprecated')
            import
            type(pj),value :: p 
            integer(kind=c_int) :: proj_is_deprecated
        end function proj_is_deprecated
    end interface
    !
    interface
        function proj_is_equivalent_to(obj,other,criterion) &
            bind(c,name='proj_is_equivalent_to')
            import
            type(pj),value :: obj
            type(pj),value :: other
            integer(kind=kind(pj_comp_strict)) :: criterion
            integer(kind=c_int) :: proj_is_equivalent_to
        end function proj_is_equivalent_to
    end interface
    !
    interface
        function proj_is_equivalent_to_with_ctx(ctx,obj,other,criterion) &
            bind(c,name='proj_is_equivalent_to_with_ctx')
            import 
            type(pj_context),value :: ctx
            type(pj),value :: obj
            type(pj),value :: other
            integer(kind=kind(pj_comp_strict)) :: criterion
            integer(kind=c_int) :: proj_is_equivalent_to_with_ctx
        end function proj_is_equivalent_to_with_ctx
    end interface
    !
    interface
        function proj_is_crs(obj) bind(c,name='proj_is_crs')
            import
            type(pj),value :: obj
            integer(kind=c_int) :: proj_is_crs
        end function proj_is_crs
    end interface
    !
    interface
        function proj_get_name(obj) bind(c,name='proj_get_name')
            import
            type(pj),value :: obj
            type(c_ptr) :: proj_get_name !This is a string
        end function proj_get_name
    end interface
    !
    interface
        function proj_as_wkt(ctx,obj,type_,options) bind(c,name='proj_as_wkt')
            import
            type(pj_context),value :: ctx
            type(pj),value :: obj
            integer(kind=kind(pj_wkt2_2015)),value :: type_
            type(c_ptr),value :: options
            type(c_ptr) :: proj_as_wkt
        end function proj_as_wkt
    end interface
    !
    interface
        function proj_as_proj_string(ctx,obj,type_,options) &
            bind(c,name='proj_as_proj_string')
            import
            type(pj_context),value :: ctx
            type(pj),value :: obj
            integer(kind=kind(pj_proj_4)),value :: type_
            type(c_ptr),value :: options
            type(c_ptr) :: proj_as_proj_string 
        end function
    end interface
    !
    interface
        function proj_as_projjson(ctx,obj,options) bind(c,name='proj_as_projjson')
            import
            type(pj_context),value :: ctx
            type(pj),value :: obj
            type(c_ptr),value :: options
            type(c_ptr) :: proj_as_projjson
        end function proj_as_projjson
    end interface
    !
    interface
        function proj_get_source_crs(ctx,obj) bind(c,name='proj_get_source_crs')
            import
            type(pj_context),value :: ctx
            type(pj),value :: obj
            type(pj) :: proj_get_source_crs
        end function proj_get_source_crs
    end interface
    !
    interface
        function proj_get_target_crs(ctx,obj) bind(c,name='proj_get_target_crs')
            import
            type(pj_context),value :: ctx
            type(pj),value :: obj
            type(pj) :: proj_get_target_crs
        end function proj_get_target_crs
    end interface
    !
    interface
        function proj_crs_is_derived(ctx,crs) bind(c,name='proj_crs_is_derived')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: crs 
            integer(kind=c_int) :: proj_crs_is_derived
        end function proj_crs_is_derived
    end interface
    !
    interface
        function proj_crs_get_geodetic_crs(ctx,crs) &
            bind(c,name='proj_crs_get_geodetic_crs')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: crs 
            type(pj) :: proj_crs_get_geodetic_crs
        end function proj_crs_get_geodetic_crs
    end interface
    !
    interface
        function proj_crs_get_horizontal_datum(ctx,crs) &
            bind(c,name='proj_crs_get_horizontal_datum')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: crs 
            type(pj) :: proj_crs_get_horizontal_datum
        end function proj_crs_get_horizontal_datum
    end interface
    !
    interface
        function proj_crs_get_sub_crs(ctx,crs,index) bind(c,name='proj_crs_get_sub_crs')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: crs 
            integer(kind=c_int) :: index
            type(pj) :: proj_crs_get_sub_crs
        end function proj_crs_get_sub_crs
    end interface
    !
    interface
        function proj_crs_get_datum(ctx,crs) bind(c,name='proj_crs_get_datum')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: crs 
            type(pj) :: proj_crs_get_datum
        end function proj_crs_get_datum
    end interface
    !
    interface
        function proj_crs_get_datum_ensemble(ctx,crs) &
            bind(c,name='proj_crs_get_datum_ensemble')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: crs 
            type(pj) :: proj_crs_get_datum_ensemble
        end function proj_crs_get_datum_ensemble
    end interface
    !
    interface
        function proj_crs_get_datum_forced(ctx,crs) &
            bind(c,name='proj_crs_get_datum_forced')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: crs 
            type(pj) :: proj_crs_get_datum_forced
        end function proj_crs_get_datum_forced
    end interface
    !
    interface
        function proj_crs_has_point_motion_operation(ctx,crs) &
            bind(c,name='proj_crs_has_point_motion_operation')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: crs 
            integer(kind=c_int) :: proj_crs_has_point_motion_operation
        end function proj_crs_has_point_motion_operation
    end interface
    !
    interface
        function proj_datum_ensemble_get_member_count(ctx,datum_ensemble) &
            bind(c,name='proj_datum_ensemble_get_member_count')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: datum_ensemble 
            integer(kind=c_int) :: proj_datum_ensemble_get_member_count
        end function proj_datum_ensemble_get_member_count
    end interface 
    !
    interface
        function proj_datum_ensemble_get_accuracy(ctx,datum_ensemble) &
            bind(c,name='proj_datum_ensemble_get_accuracy')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: datum_ensemble 
            real(kind=c_double) :: proj_datum_ensemble_get_accuracy
        end function proj_datum_ensemble_get_accuracy
    end interface
    !
    interface
        function proj_datum_ensemble_get_member(ctx,datum_ensemble,member_index) &
            bind(c,name='proj_datum_ensemble_get_member')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: datum_ensemble
            integer(kind=c_int),value :: member_index 
            type(pj) :: proj_datum_ensemble_get_member
        end function proj_datum_ensemble_get_member
    end interface
    !
    interface
        function proj_dynamic_datum_get_frame_reference_epoch(ctx,datum) &
            bind(c,name='proj_dynamic_datum_get_frame_reference_epoch')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: datum
            real(kind=c_double) ::  proj_dynamic_datum_get_frame_reference_epoch
        end function  proj_dynamic_datum_get_frame_reference_epoch
    end interface
    !
    interface
        function proj_crs_get_coordinate_system(ctx,crs) &
            bind(c,name='proj_crs_get_coordinate_system')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: crs
            type(pj) :: proj_crs_get_coordinate_system
        end function  proj_crs_get_coordinate_system
    end interface
    !
    interface
        function proj_cs_get_type(ctx,cs) bind(c,name='proj_cs_get_type')
            import
            type(pj_context),value :: ctx 
            type(pj),value :: cs
            integer(kind=kind(pj_cs_type_unknown)) :: proj_cs_get_type
        end function  proj_cs_get_type
    end interface
    !
    interface
        function proj_cs_get_axis_count(ctx,cs) bind(c,name='proj_cs_get_axis_count')
            import
            type(pj_context),value :: ctx
            type(pj),value :: cs
            integer(kind=c_int) :: proj_cs_get_axis_count
        end function proj_cs_get_axis_count 
    end interface
	!
    ! PROJ context
    !
    interface
        function proj_context_create() bind(c,name='proj_context_create')
            import
            type(pj_context) :: proj_context_create
        end function proj_context_create
    end interface

    interface
        function proj_context_destroy(ctx) bind(c,name='proj_context_destroy')
            import
            type(pj_context),value :: ctx
            type(pj_context) :: proj_context_destroy
        end function proj_context_destroy
    end interface

    interface
        function proj_context_clone(ctx) bind(c,name='proj_context_clone')
            import
            type(pj_context),value :: ctx
            type(pj_context) :: proj_context_clone
        end function proj_context_clone
    end interface
    !
    interface
        subroutine proj_area_set_bbox(area, west_lon_degree, south_lat_degree, &
        east_lon_degree, north_lat_degree) bind(c,name='proj_area_set_bbox')
            import
            type(pj_area),value :: area
            real(kind=c_double),value :: west_lon_degree
            real(kind=c_double),value :: south_lat_degree
            real(kind=c_double),value :: east_lon_degree
            real(kind=c_double),value :: north_lat_degree
        end subroutine proj_area_set_bbox
    end interface

    interface
        subroutine proj_area_destroy(area) bind(c,name='proj_area_destroy')
            import
            type(pj_area),value :: area
        end subroutine proj_area_destroy
    end interface
    !
	interface
        function proj_trans(p, direction, coord) bind(c,name='proj_trans')
            import
            type(pj),value :: p
            integer(kind=kind(pj_fwd)),value :: direction ! warning this is an enum
            type(pj_coord),value :: coord
            type(pj_coord) :: proj_trans
        end function proj_trans
	end interface
    !
    interface
        function proj_trans_array(p, direction, n, coord) bind(c,name='proj_trans_array')
            import
            type(pj),value :: p
            integer(kind=kind(pj_fwd)),value :: direction ! warning this is an enum
            integer(kind=c_size_t),value :: n
            type(pj_coord) :: coord(*)
            integer(kind=c_int) :: proj_trans_array
        end function proj_trans_array
    end interface
    !
    interface
        function proj_info() bind(c,name='proj_info')
            import
            type(pj_info) :: proj_info
        end function proj_info
    end interface
    !
    interface
        function proj_pj_info(p) bind(c,name='proj_pj_info')
            import
            type(pj),value :: p
            type(pj_proj_info) :: proj_pj_info
        end function proj_pj_info
    end interface
    !
    interface
        function proj_get_type(obj) bind(c,name='proj_get_type')
            import
            type(pj),value :: obj
            integer(kind=kind(pj_type_unknown)) :: proj_get_type
        end function proj_get_type
    end interface
    !
    interface
        function proj_torad(angle_in_degrees) bind(c,name='proj_torad')
            import
            real(kind=c_double) :: angle_in_degrees
            real(kind=c_double) :: proj_torad
        end function proj_torad
    end interface
    !
    interface
        function proj_todeg(angle_in_radians) bind(c,name='proj_todeg')
            import
            real(kind=c_double) :: angle_in_radians
            real(kind=c_double) :: proj_todeg
        end function proj_todeg
    end interface
    !
    interface
        function proj_errno(p) bind(c,name='proj_errno')
            import
            type(pj),value :: p
            integer(kind=c_int) :: proj_errno
        end function proj_errno
    end interface
    !
    interface
        function proj_context_errno(ctx) bind(c,name='proj_context_errno')
            import
            type(pj_context),value :: ctx
            integer(kind=c_int) :: proj_context_errno
        end function proj_context_errno
    end interface
    !
    interface
        function proj_errno_string(err) bind(c,name='proj_errno_string')
            import
            integer(kind=c_int),value :: err
            type(c_ptr) :: proj_errno_string
        end function proj_errno_string
    end interface
    !
    interface
        function proj_context_errno_string(ctx, err) bind(c,name='proj_context_errno_string')
            import
            type(pj_context),value :: ctx
            integer(kind=c_int),value :: err
            type(c_ptr) :: proj_context_errno_string
        end function proj_context_errno_string
    end interface
    !
    interface
        function proj_string_list_destroy(list) bind(c,name='proj_string_list_destroy')
            import
            type(pj_string_list),value :: list
            type(c_ptr) :: proj_string_list_destroy
        end function proj_string_list_destroy
    end interface
    !
    interface
        function proj_create_from_wkt(ctx,wkt,options,out_warnings,&
            out_grammar_errors) bind(c,name='proj_create_from_wkt')
            import
            type(pj_context),value :: ctx
            type(c_ptr) :: wkt
            type(c_ptr) :: options
            type(pj_string_list),value :: out_warnings
            type(pj_string_list),value :: out_grammar_errors
            type(pj) :: proj_create_from_wkt
        end function proj_create_from_wkt
    end interface
    !
    interface proj_associated
      module procedure proj_associated_pj, &
        proj_associated_context, &
        proj_associated_area
    end interface proj_associated
    !
    interface strtofchar
      !module procedure strtofchar_char, strtofchar_chararr, strtofchar_intarr, &
       module procedure ::strtofchar_ptr_2
    end interface
    !
    interface strlen
      !module procedure strlen_char, strlen_chararr, strlen_intarr, &
        module procedure strlen_ptr
    end interface
    !
    interface print_info
        module procedure print_info_pj,print_info_pj_info
    end interface print_info
    !
    contains
    !
        function proj_associated_pj(object) result(associated_)
            type(pj),intent(in) :: object
            logical :: associated_
            associated_ = c_associated(object%ptr)
        end function proj_associated_pj
        !
        function proj_associated_context(object) result(associated_)
            type(pj_context),intent(in) :: object
            logical :: associated_
            associated_ = c_associated(object%ptr)
        end function proj_associated_context
        !
        function proj_associated_area(object) result(associated_)
            type(pj_area),intent(in) :: object
            logical :: associated_
            associated_ = c_associated(object%ptr)
        end function proj_associated_area
        !
        function proj_trans_f(p, direction, coord)
            type(pj),value :: p 
            integer(kind=kind(pj_fwd)),value :: direction 
            type(pj_coord) :: coord(:)
            integer :: proj_trans_f

            integer(kind=c_size_t) :: n

            n = size(coord)
            write(*,*) 'n= ',n,direction;
            proj_trans_f = proj_trans_array(p, direction, n, coord)

        end function proj_trans_f
        !
        function proj_trans_f1(p, direction, coord)
            type(pj),value :: p
            integer(kind=kind(pj_fwd)),value :: direction 
            type(pj_coord) :: coord(:)
            integer :: proj_trans_f1

            integer(kind=c_size_t) :: n
            integer :: i
            type(pj_coord) :: temp(size(coord))

            n = size(coord);
            write(*,*) 'n= ',n,direction;
            do i=1,n;
                temp(i)=proj_trans(p, direction, coord(i));
            end do
            coord=temp;

        end function proj_trans_f1
        !
        function proj_trans_f2(pj_obj, direction, x_in, y_in, x_out, y_out) result(res)
            type(pj),value :: pj_obj
            integer(kind=kind(pj_fwd)),value :: direction
            real(kind=wp),dimension(:),intent(in) :: x_in,y_in
            real(kind=wp),dimension(:),intent(out) :: x_out,y_out
            integer :: res
            !
            type(pj_coord),dimension(size(x_in,dim=1)) :: coords
            integer(kind=c_size_t) :: n
            !
            n=size(x_in,dim=1);
            coords(:)%x=x_in;
            coords(:)%y=y_in;
            coords(:)%z=0.0_c_double;
            coords(:)%t=0.0_c_double;
            !
            res=proj_trans_array(pj_obj, direction, n, coords);
            !
            x_out=coords(:)%x;
            y_out=coords(:)%y;
            !
        end function proj_trans_f2
        !
        function proj_trans_coord_array(pj_obj, direction, coord_array, coords) &
            result(res)
            type(pj),value :: pj_obj
            integer(kind=kind(pj_fwd)),value :: direction
            real(kind=wp),dimension(:,:) :: coord_array
            type(pj_coord),dimension(size(coord_array,dim=1)) :: coords 
            integer :: res 
            !
            integer :: i
            integer(kind=c_size_t) :: n 
            !
            n=size(coord_array,dim=1);
            coords(:)%x=coord_array(:,1);
            coords(:)%y=coord_array(:,2);
            res=proj_trans_array(pj_obj, direction, n, coords);
            !
        end function proj_trans_coord_array
        !
        function strlen_ptr(string) result(strlen)
            type(c_ptr),intent(in) :: string
            integer :: strlen

            integer(kind=c_signed_char),pointer :: pstring(:)
            integer :: i

            if (c_associated(string)) then ! conflicts with pure
            ! null c pointer does not produce unassociated fortran pointer with intel
              call c_f_pointer(string, pstring, (/huge(i)/))
            ! if (associated(pstring)) then
              do i = 1, size(pstring)
                if (pstring(i) == 0) exit
              enddo
              strlen = i - 1
            else
              strlen = 0
            endif

        end function strlen_ptr
        !
        function strtofchar_ptr_2(string, fixlen) result(fchar)
            type(c_ptr),intent(in) :: string
            integer,intent(in) :: fixlen
            character(len=fixlen) :: fchar

            character(len=fixlen),pointer :: pfchar
            integer :: safelen

            safelen = min(strlen(string), fixlen)

            fchar = '';
            if (c_associated(string)) then
              call c_f_pointer(string, pfchar)
              fchar(1:safelen) = pfchar(1:safelen)
            endif

        end function strtofchar_ptr_2
        !
        function c_ptr_to_char(cstr,max_len) result(fstr)
            type(c_ptr),intent(in) :: cstr
            integer,intent(in) :: max_len
            character(len=max_len) :: fstr
            !
            type(c_ptr) :: ptr
            integer(kind=c_int) :: len,i,n
            character(kind=c_char),pointer :: c_array(:)
            !
            fstr='';
            if(.not. c_associated(cstr)) then
                write(*,*) "Error: NULL C pointer passed to c_ptr_to_char()";
                stop;
            end if
            n=max_len;
            ptr=cstr;
            call c_f_pointer(ptr,c_array,[n]);
            len=0;
            do i=1,size(c_array);
                if(c_array(i) == c_null_char) exit;
                len=len+1; 
            end do
            fstr=transfer(c_array(1:len),fstr);
            !
        end function c_ptr_to_char
        !
        subroutine print_info_pj(info)
            type(pj_info),intent(in) :: info 
            !
            write(*,'(A)') '==== proj information ====';
            write(*,'(A,1X,A)') 'version: ', trim(strtofchar(info%version,NUMCHAR));
            write(*,'(A,1X,A)') 'release: ', trim(strtofchar(info%release,NUMCHAR));
            write(*,'(A,1X,A)') 'searchpath: ', trim(strtofchar(info%searchpath,NUMCHAR));
            write(*,'(A,1X,I10)') 'major: ',info%major;
            write(*,'(A,1X,I10)') 'minor: ',info%minor;
            write(*,'(A,1X,I10)') 'patch: ',info%patch;
            write(*,'(A)') '==========================';
            !
        end subroutine print_info_pj
        !        
        subroutine print_info_pj_info(info)
            type(pj_proj_info),intent(in) :: info
            !
            write(*,'(A)') '==== proj object information ====';
            write(*,'(A,1X,A)') 'id:',trim(strtofchar(info%id,NUMCHAR));
            write(*,'(A,1X,A)') 'desc:',trim(strtofchar(info%description,NUMCHAR));
            write(*,'(A,1X,A)') 'def:',trim(strtofchar(info%definition,NUMCHAR));
            write(*,'(A,1X,I5)') 'has inv:',info%has_inverse;
            write(*,'(A,1X,f10.3)') 'accuracy:',info%accuracy;
            write(*,'(A)') '================================';
            !
        end subroutine print_info_pj_info
    !
end module fproj_utilities