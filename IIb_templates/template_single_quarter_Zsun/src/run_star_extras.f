! ***********************************************************************
!
!   Copyright (C) 2012  Bill Paxton
!
!   this file is part of mesa.
!
!   mesa is free software; you can redistribute it and/or modify
!   it under the terms of the gnu general library public license as published
!   by the free software foundation; either version 2 of the license, or
!   (at your option) any later version.
!
!   mesa is distributed in the hope that it will be useful, 
!   but without any warranty; without even the implied warranty of
!   merchantability or fitness for a particular purpose.  see the
!   gnu library general public license for more details.
!
!   you should have received a copy of the gnu library general public license
!   along with this software; if not, write to the free software
!   foundation, inc., 59 temple place, suite 330, boston, ma 02111-1307 usa
!
! ***********************************************************************
 
      module run_star_extras 

      use star_lib
      use star_def
      use const_def
      use chem_def
      use num_lib
      
      implicit none

      ! these routines are called by the standard run_star check_model
      contains
      
      subroutine extras_controls(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         
         ! this is the place to set any procedure pointers you want to change
         ! e.g., other_wind, other_mixing, other_energy  (see star_data.inc)
         
         ! Uncomment these lines if you wish to use the functions in this file,
         ! otherwise we use a null_ version which does nothing.
         s% extras_startup => extras_startup
         s% extras_check_model => extras_check_model
         s% extras_finish_step => extras_finish_step
         s% extras_after_evolve => extras_after_evolve
         s% how_many_extra_history_columns => how_many_extra_history_columns
         s% data_for_extra_history_columns => data_for_extra_history_columns
         s% how_many_extra_profile_columns => how_many_extra_profile_columns
         s% data_for_extra_profile_columns => data_for_extra_profile_columns  
         s% other_wind => dutch_scaled_Z

         ! Once you have set the function pointers you want,
         ! then uncomment this (or set it in your star_job inlist)
         ! to disable the printed warning message,
          s% job% warn_run_star_extras =.false.       
            
      end subroutine extras_controls
      
      ! None of the following functions are called unless you set their
      ! function point in extras_control.
      
      
      integer function extras_startup(id, restart, ierr)
         integer, intent(in) :: id
         logical, intent(in) :: restart
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_startup = 0
         if (.not. restart) then
            call alloc_extra_info(s)
         else ! it is a restart
            call unpack_extra_info(s)
         end if
         s% xtra1 = 0
         s% xtra2 = 0
      end function extras_startup
      

      ! returns either keep_going, retry, backup, or terminate.
      integer function extras_check_model(id, id_extra)
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_check_model = keep_going         
         !write(*,*) "check mass", s% star_mass, s% he_core_mass,&
         !   s% star_mass - s% he_core_mass, 0.3*s% star_mass, &
         !   (s% star_mass - s% he_core_mass) < 0.3*s% star_mass
         !if ((s% star_mass - s% he_core_mass) < 0.3*s% star_mass &
         !   .and. .not. s% okay_to_reduce_gradT_excess) then
         !   s% okay_to_reduce_gradT_excess = .true.
         !   write(*,*) "turn on MLT++"
         !end if


         ! if you want to check multiple conditions, it can be useful
         ! to set a different termination code depending on which
         ! condition was triggered.  MESA provides 9 customizeable
         ! termination codes, named t_xtra1 .. t_xtra9.  You can
         ! customize the messages that will be printed upon exit by
         ! setting the corresponding termination_code_str value.
         ! termination_code_str(t_xtra1) = 'my termination condition'

         ! by default, indicate where (in the code) MESA terminated
         if (extras_check_model == terminate) s% termination_code = t_extras_check_model
      end function extras_check_model


      integer function how_many_extra_history_columns(id, id_extra)
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_history_columns = 2
      end function how_many_extra_history_columns
      
      
      subroutine data_for_extra_history_columns(id, id_extra, n, names, vals, ierr)
         integer, intent(in) :: id, id_extra, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         names(1) = "mass_tr"
         vals(1) = s% xtra1
         names(2) = "mass_ac"
         vals(2) = s% xtra2
         
         !note: do NOT add the extras names to history_columns.list
         ! the history_columns.list is only for the built-in log column options.
         ! it must not include the new column names you are adding here.
         

      end subroutine data_for_extra_history_columns

      
      integer function how_many_extra_profile_columns(id, id_extra)
         use star_def, only: star_info
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_profile_columns = 0
      end function how_many_extra_profile_columns
      
      
      subroutine data_for_extra_profile_columns(id, id_extra, n, nz, names, vals, ierr)
         use star_def, only: star_info, maxlen_profile_column_name
         use const_def, only: dp
         integer, intent(in) :: id, id_extra, n, nz
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(nz,n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         integer :: k
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         
         !note: do NOT add the extra names to profile_columns.list
         ! the profile_columns.list is only for the built-in profile column options.
         ! it must not include the new column names you are adding here.

         ! here is an example for adding a profile column
         !if (n /= 1) stop 'data_for_extra_profile_columns'
         !names(1) = 'beta'
         !do k = 1, nz
         !   vals(k,1) = s% Pgas(k)/s% P(k)
         !end do
         
      end subroutine data_for_extra_profile_columns
      

      ! returns either keep_going or terminate.
      ! note: cannot request retry or backup; extras_check_model can do that.
      integer function extras_finish_step(id, id_extra)
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_finish_step = keep_going
         call store_extra_info(s)
         !call binary_ptr(s% binary_id, b, ierr)
         if (ierr /= 0) return

         !s% xtra1 = s% xtra1 - b% mtransfer_rate*s% dt/Msun
         !s% xtra2 = s% xtra2 - b% xfer_fraction * b% mtransfer_rate*s% dt/Msun
         !if(s% id == 1) then
         !   write(*,*) "mass transferred", s% xtra1
         !   write(*,*) "mass accreted", s% xtra2
         !end if

         if(abs(s% xtra25_old - s% center_h1) > 0.0025) then
             s% dt_next = min(s% dt_next, s% dt * s% min_timestep_factor)
             write(*,*) "reducing dt due to large change in central hydrogen"
          else if(abs(s% xtra26_old - s% center_he4) > 0.0025) then
             s% dt_next = min(s% dt_next, s% dt * s% min_timestep_factor)
             write(*,*) "reducing dt due to large change in central helium"
          else if(abs(s% xtra27_old - s% center_c12) > 0.0025) then
             s% dt_next = min(s% dt_next, s% dt * s% min_timestep_factor)
             write(*,*) "reducing dt due to large change in central carbon"
         end if
         s% xtra25 = s% center_h1
         s% xtra26 = s% center_he4
         s% xtra27 = s% center_c12
         
         ! terminate when carbon is running low
         if (s% center_he4 < 1d-6 .and. s% center_c12 < 0.001) then
            extras_finish_step = terminate
            write(*,*) "Terminate: reached central lower limit for c12"
         end if


         ! to save a profile, 
            ! s% need_to_save_profiles_now = .true.
         ! to update the star log,
            ! s% need_to_update_history_now = .true.

         ! see extras_check_model for information about custom termination codes
         ! by default, indicate where (in the code) MESA terminated
         if (extras_finish_step == terminate) s% termination_code = t_extras_finish_step
      end function extras_finish_step
      
      
      subroutine extras_after_evolve(id, id_extra, ierr)
         integer, intent(in) :: id, id_extra
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
      end subroutine extras_after_evolve
      
      
      ! routines for saving and restoring extra data so can do restarts
         
         ! put these defs at the top and delete from the following routines
         !integer, parameter :: extra_info_alloc = 1
         !integer, parameter :: extra_info_get = 2
         !integer, parameter :: extra_info_put = 3
      
      
      subroutine alloc_extra_info(s)
         integer, parameter :: extra_info_alloc = 1
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_alloc)
      end subroutine alloc_extra_info
      
      
      subroutine unpack_extra_info(s)
         integer, parameter :: extra_info_get = 2
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_get)
      end subroutine unpack_extra_info
      
      
      subroutine store_extra_info(s)
         integer, parameter :: extra_info_put = 3
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_put)
      end subroutine store_extra_info
      
      
      subroutine move_extra_info(s,op)
         integer, parameter :: extra_info_alloc = 1
         integer, parameter :: extra_info_get = 2
         integer, parameter :: extra_info_put = 3
         type (star_info), pointer :: s
         integer, intent(in) :: op
         
         integer :: i, j, num_ints, num_dbls, ierr
         
         i = 0
         ! call move_int or move_flg    
         num_ints = i
         
         i = 0
         ! call move_dbl       
         
         num_dbls = i
         
         if (op /= extra_info_alloc) return
         if (num_ints == 0 .and. num_dbls == 0) return
         
         ierr = 0
         call star_alloc_extras(s% id, num_ints, num_dbls, ierr)
         if (ierr /= 0) then
            write(*,*) 'failed in star_alloc_extras'
            write(*,*) 'alloc_extras num_ints', num_ints
            write(*,*) 'alloc_extras num_dbls', num_dbls
            stop 1
         end if
         
         contains
         
         subroutine move_dbl(dbl)
            real(dp) :: dbl
            i = i+1
            select case (op)
            case (extra_info_get)
               dbl = s% extra_work(i)
            case (extra_info_put)
               s% extra_work(i) = dbl
            end select
         end subroutine move_dbl
         
         subroutine move_int(int)
            integer :: int
            i = i+1
            select case (op)
            case (extra_info_get)
               int = s% extra_iwork(i)
            case (extra_info_put)
               s% extra_iwork(i) = int
            end select
         end subroutine move_int
         
         subroutine move_flg(flg)
            logical :: flg
            i = i+1
            select case (op)
            case (extra_info_get)
               flg = (s% extra_iwork(i) /= 0)
            case (extra_info_put)
               if (flg) then
                  s% extra_iwork(i) = 1
               else
                  s% extra_iwork(i) = 0
               end if
            end select
         end subroutine move_flg
      
      end subroutine move_extra_info

      subroutine dutch_scaled_Z(id, Lsurf, Msurf, Rsurf, Tsurf, w, ierr)
         use star_def
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         !call star_ptr(id, s, ierr)
         !if (ierr /= 0) return
         integer, intent(in) :: id
         real(dp), intent(in) :: Lsurf, Msurf, Rsurf, Tsurf ! surface values (cgs)
         ! NOTE: surface is outermost cell. not necessarily at
         ! photosphere.
         ! NOTE: don't assume that vars are set at this point.
         ! so if you want values other than those given as args,
         ! you should use values from s% xh(:,:) and s% xa(:,:) only.
         ! rather than things like s% Teff or s% lnT(:) which have not
         ! been set yet.
         real(dp), intent(out) :: w ! wind in units of Msun/year (value is >= 0)
         integer :: h1, he4
         real(dp) :: k, nz, alfa, T_high, T_low, w1, w2, wind, &
            L1, M1, R1, T1, X, Y, Z, surface_h1, surface_he4, &
            sup_edd_scale, Ledd, Zpop
         logical :: edd_limit
         real(dp), parameter :: Zsolar = 0.019d0 ! for Vink et al formula
         logical, parameter :: dbg = .false.
         include 'formats'
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         T_high = 11000
         T_low = 10000
         L1 = Lsurf
         M1 = Msurf
         T1 = Tsurf
         R1 = sqrt(L1/(pi*crad*clight*T1*T1*T1*T1)) ! assume L1 and T1 for photosphere
         h1 = s% net_iso(ih1)
         he4 = s% net_iso(ihe4)
         if (h1 > 0) then
            surface_h1 = s% xa(h1,1)
         else
            surface_h1 = 0
         end if
         if (he4 > 0) then
            surface_he4 = s% xa(he4,1)
         else
            surface_he4 = 0
         end if
         X = surface_h1
         Y = surface_he4
         Z = 1 - (X + Y)
         Zpop = s% Zbase/0.02d0
         if (T1 <= T_low) then
            call eval_lowT_Dutch(wind)
         else if (T1 >= T_high) then
            call eval_highT_Dutch(wind)
         else ! transition
            call eval_lowT_Dutch(w1)
            call eval_highT_Dutch(w2)
            alfa = (T1 - T_low)/(T_high - T_low)
            wind = (1-alfa)*w1 + alfa*w2
         end if

         w = 1.0 * wind ! SCALE WINDS HERE

         contains

         subroutine eval_lowT_Dutch(w)
            real(dp), intent(out) :: w
            include 'formats'
            call eval_de_Jager_wind(w)
            if (dbg) write(*,1) 'Dutch_wind = de Jager', &
               log10_cr(wind), T1, T_low, T_high
         end subroutine eval_lowT_Dutch

         subroutine eval_de_Jager_wind(w)
            ! de Jager, C., Nieuwenhuijzen, H., & van der Hucht, K. A.
            ! 1988, A&AS, 72, 259.
            real(dp), intent(out) :: w
            real(dp) :: log10w
            include 'formats'
            log10w = 1.769d0*log10_cr(L1/Lsun) - 1.676d0*log10_cr(T1) &
               - 8.158d0 + 0.85d0*log10_cr(Zpop) !slapping Z dependence
            w = exp10_cr(log10w)
            if (dbg) then
               write(*,1) 'de_Jager log10 wind', log10w
            end if
         end subroutine eval_de_Jager_wind

         subroutine eval_highT_Dutch(w)
            real(dp), intent(out) :: w
            include 'formats'
            if (surface_h1 < 0.4d0) then ! helium rich Wolf-Rayet star: Nugis & Lamers
               w = 1d-11 * pow_cr(L1/Lsun,1.29d0) * pow_cr(Y,1.7d0) * sqrt(Z)
               if (dbg) write(*,1) 'Dutch_wind = Nugis & Lamers', &
                  log10_cr(wind)
            else
               call eval_Vink_wind(w)
            end if
         end subroutine eval_highT_Dutch

         subroutine eval_Vink_wind(w)
            real(dp), intent(inout) :: w
            real(dp) :: alfa, w1, w2, Teff_jump, logMdot, dT, &
               vinf_div_vesc
            ! alfa = 1 for hot side, = 0 for cool side
            if (T1 > 27500d0) then
               alfa = 1
            else if (T1 < 22500d0) then
               alfa = 0
            else ! use Vink et al 2001, eqns 14 and 15 to set "jump" temperature
               Teff_jump = 1d3*(61.2d0 + 2.59d0*(-13.636d0 + &
                  0.889d0*log10_cr(Z/Zsolar)))
               dT = 100d0
               if (T1 > Teff_jump + dT) then
                  alfa = 1
               else if (T1 < Teff_jump - dT) then
                  alfa = 0
               else
                  alfa = (T1 - (Teff_jump - dT)) / (2*dT)
               end if
            end if

            if (alfa > 0) then ! eval hot side wind (eqn 24)
               vinf_div_vesc = 2.6d0 ! this is the hot side galactic value
               vinf_div_vesc = vinf_div_vesc*pow_cr(Z/Zsolar,0.13d0) ! corrected for Z
               logMdot = &
                  - 6.697d0 &
                  + 2.194d0*log10_cr(L1/Lsun/1d5) &
                  - 1.313d0*log10_cr(M1/Msun/30) &
                  - 1.226d0*log10_cr(vinf_div_vesc/2d0) &
                  + 0.933d0*log10_cr(T1/4d4) &
                  - 10.92d0*pow2(log10_cr(T1/4d4)) &
                  + 0.85d0*log10_cr(Z/Zsolar)
               w1 = exp10_cr(logMdot)
            else
               w1 = 0
            end if

            if (alfa < 1) then ! eval cool side wind (eqn 25)
               vinf_div_vesc = 1.3d0 ! this is the cool side galactic value
               vinf_div_vesc = vinf_div_vesc*pow_cr(Z/Zsolar,0.13d0) ! corrected for Z
               logMdot = &
                  - 6.688d0 &
                  + 2.210d0*log10_cr(L1/Lsun/1d5) &
                  - 1.339d0*log10_cr(M1/Msun/30) &
                  - 1.601d0*log10_cr(vinf_div_vesc/2d0) &
                  + 1.07d0*log10_cr(T1/2d4) &
                  + 0.85d0*log10_cr(Z/Zsolar)
               w2 = exp10_cr(logMdot)
            else
               w2 = 0
            end if

            w = alfa*w1 + (1 - alfa)*w2
            if (dbg) write(*,*) 'vink wind', w
         end subroutine eval_Vink_wind

      end subroutine dutch_scaled_Z

      end module run_star_extras
      
