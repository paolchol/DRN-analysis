SUBROUTINE routing_new(STATUS)
!Till: computationally irrelevant: removed unused parameters flow, rarea
!2011-05-05

!Till: computationally irrelevant: quick-fix of severe performance loss when using output of river_flow.dat in hourly version
!2009-04-02

!Till: added output for River_Sediment_Storage.out
!2008-11-13

!(George:) various changes concerning routing of reservoir fluxes
!2008-10-07

!Till: file and header of river_sediment_total was not created when river_transport=3
!2008-07-11

!! Routing of water and sediment fluxes through the river system
!!
!! Status 0: reads input files and initialises run, calls subroutine reservoir (status 0)
!! Status 1: initialises year, calls subroutine reservoir (status 1)
!! Status 2: main calculations, calls subroutine reservoir (status 2)
!! Status 3: finalises year, calls subroutine reservoir (status 3)
!! Parameter file: routing_h.f90

!!	Q_spring	(m3/s)		spring inflow at the start of a river

use lake_h
use climo_h
use common_h
use hymo_h
use params_h
use routing_h
use time_h
use reservoir_h
use erosion_h
use utils_h

IMPLICIT NONE

INTEGER, INTENT(IN) :: STATUS
!INTEGER :: bat
INTEGER :: idummy !,imun,imunx,irout,irout2,irout_d,id,imeso,istate ! id: additional loop variable of days (total 7 days)
INTEGER :: upstream, downstream
INTEGER :: i, j, h,k, istate !itl, itr, ih, mm, imunout, iout,  make
!REAL :: xdum(48),check,temp2,qtemp, storcapact, con_sed
Real :: temp_water(subasin), temp_sediment(subasin)
character(len=1000) :: fmtstr	!string for formatting file output
Real :: r_sediment_storage(subasin)		!(suspended) sediment storage in reach [t] (for output only)
logical :: log_temp !temporary logical variable needed for compiler compatibility

! -----------------------------------------------------------------------
IF (STATUS == 0) THEN

! READ INPUT FILES
! Read hydrological response and river parameters
 OPEN(11,FILE=pfadp(1:pfadj)// 'River/river.dat', IOSTAT=istate,STATUS='old')
        IF (istate/=0) THEN
            write(*,*) "ERROR: ", pfadp(1:pfadj)// 'River/river.dat could not be opened. Aborting.'
            stop
        END IF
  READ (11,*); READ(11,*)

 r_depth(:)=-1. !for checking completeness
 DO WHILE (.TRUE.)
            READ(11,'(a)', IOSTAT=istate) fmtstr

			IF (istate/=0) THEN    !no further line
                exit        !exit loop
            END IF
            READ(fmtstr,*, IOSTAT=istate) i
			k=id_ext2int(i, id_subbas_extern)    !convert to internal subbas id
            IF (k==-1) THEN    !ID not found
                write(*,'(A,i0,A)')'WARNING: Unknown SUBBAS-ID ', i,' in river.dat, ignored.'
				cycle
             END IF
            READ (fmtstr,*, IOSTAT=istate) i,r_depth(k),r_width(k), r_sideratio(k),r_width_fp(k),r_sideratio_fp(k), &
   r_slope(k), r_length(k),manning(k), manning_fp(k),r_ksat(k),r_efactor(k),r_cover(k),r_rock(k),r_alpha(k), &
   msk_x(k), msk_k(k),Q_spring(k)

            IF (istate/=0) THEN    !no further line
                write(*,'(A,i0,A)')'ERROR: format error in river.dat.'
                stop
            END IF

            if (r_efactor(k) < 0 .OR. r_efactor(k)>1) then
                write(*,'(A,i0,A)')'WARNING: (river.dat, subbasin ',i,') river erodibility factor must be in [0..1], truncated.'
                r_efactor(k)=min(1., max(0., r_efactor(k)))
            end if

            if (r_cover(k) < 0 .OR. r_cover(k)>1) then
                write(*,'(A,i0,A)')'WARNING: (river.dat, subbasin ',i,') river cover factor must be in [0..1], truncated.'
                r_cover(k)=min(1., max(0., r_cover(k)))
            end if

 END DO

 i=which1(r_depth(1:subasin) == -1.) !check for non-specified subbasins
 if (i/=0) then
	write(*,'(A,i0,A)')'ERROR: SUBBAS-ID ', id_subbas_extern(i),' missing in river.dat.'
    stop
 END IF

 CLOSE (11)

! READ INPUT FILES
! Read configuration of river system (in routing order)
OPEN(11,FILE=pfadp(1:pfadj)// 'River/routing.dat',STATUS='old')! upbasin: MAP ID of upstream sub-basin (MAP IDs);! downbasin: MAP ID of downstream sub-basin (MAP IDs)
READ (11,*); READ(11,*)
DO i=1,subasin
  READ (11,*)  idummy, upbasin(i),downbasin(i)
END DO
CLOSE (11)


! if bedload modelleing is switched on
if(river_transport.eq.3) then
  OPEN(11,FILE=pfadp(1:pfadj)// 'River/bedload.dat',STATUS='old')!
  read(11,*); read(11,*)
  DO i=1,subasin
	READ (11,*) idummy, D50(i)
	IF (idummy /= id_subbas_extern(i)) THEN
		WRITE(*,*) 'ERROR: Sub-basin-IDs in file bedload.dat must have the same ordering scheme as in hymo.dat'
		STOP
	END IF
  END DO
endif

!this relates the MAP IDs (id_subbas_extern(subasin)) to the sorted CODE IDs (id_subbas_intern(subasin)), i.e. upbasin and downbasin are now numbered according to internal ids
!so that in routing.dat only the MAP IDs have to be read in, first for the ID of upstream subasin
DO i=1,subasin
  j=1
  DO WHILE (id_subbas_extern(j) /= upbasin(i))
    j=j+1
    IF (j > 1000) THEN
      WRITE (*,*) 'ERROR: upbasin(i) loop in routing_new.f'
      STOP
    END IF
  END DO
  upbasin(i)=j
END DO
! second for the ID of downstream subasin
DO i=1,subasin
  IF (downbasin(i) /= 999.AND.downbasin(i) /= 9999) THEN
    j=1
    DO WHILE (id_subbas_extern(j) /= downbasin(i))
      j=j+1
      IF (j > 1000) THEN
        WRITE (*,*) 'ERROR: downbasin(i) loop in routing_new.f'
        STOP
      END IF
    END DO
    downbasin(i)=j
  END IF
END DO

!allocate memory for subbasin in- and outflow
allocate( qout(366 + size(hrout,dim=1) , subasin)) !
qout(:,:)=0.


! INITIALISATION OF RESERVOIR MODULE
!George (status,upstream,h) instead (status,upstream)
  IF (doreservoir) CALL reservoir (status,upstream,h)


! INITIALISATION OF OUTPUT FILES



if ((river_transport == 2)) then
  !  output of 'River_Sediment_total.out' handled in hymo_all.f90
  !  output of 'River_Sediment_Storage.out' handled in hymo_all.f90

  OPEN(11,FILE=pfadn(1:pfadi)//'River_Sediment_Concentration.out',STATUS='replace')
  if (f_river_sediment_concentration) then
	WRITE (11,*) 'Output file for sediment concentration (g/l) (with MAP IDs as in hymo.dat)'
	write(fmtstr,'(a,i0,a)')'(3a6,',subasin,'i14)'		!generate format string
	WRITE (11,fmtstr)' Year ', ' Day  ','  dt  ', (id_subbas_extern(i), i=1,subasin)
	!WRITE (11,'(3a6,<subasin>i14)')' Year ', ' Day  ','  dt  ', (id_subbas_extern(i), i=1,subasin)
	Close (11)
  else
    close(11, status='delete') !delete any existing file, if no output is desired
  endif

elseif (river_transport.eq.3) then
  if (f_river_bedload) then
	OPEN(11,FILE=pfadn(1:pfadi)//'River_bedload.out',STATUS='replace')
	WRITE (11,*) 'Output file for river bedload rate (kg/s) as submerged weight(with MAP IDs as in hymo.dat)'
	WRITE (11,*) ' Year ', ' Day  ',' dt   ',&
		'Meyer_Peter, Schoklitsch, Smart&Jaeggi, Bagnold, Rickenmann for each subasin in successive columns'
	Close (11)
  else
    close(11, status='delete') !delete any existing file, if no output is desired
  endif
endif

  OPEN(11,FILE=pfadn(1:pfadi)//'River_Velocity.out',STATUS='replace')
  if (f_river_velocity) then
	WRITE (11,*) 'Output file for flow velocity in m/s (with MAP IDs as in hymo.dat)'
	write(fmtstr,'(a,i0,a)')'(3a6,',subasin,'i14)'		!generate format string
	WRITE (11,fmtstr)' Year ', ' Day  ',' dt   ',(id_subbas_extern(i), i=1,subasin)
	Close (11)
  else
    close(11, status='delete') !delete any existing file, if no output is desired
  endif

  OPEN(11,FILE=pfadn(1:pfadi)//'River_Flowdepth.out',STATUS='replace')
  if (f_river_flowdepth) then
	WRITE (11,*) 'Output file for flow depth in m (with MAP IDs as in hymo.dat)'
	write(fmtstr,'(a,i0,a)')'(3a6,',subasin,'i14)'		!generate format string
	WRITE (11,fmtstr)' Year ', ' Day  ','  dt  ', (id_subbas_extern(i), i=1,subasin)
	Close (11)
  else
    close(11, status='delete') !delete any existing file, if no output is desired
  endif

  OPEN(11,FILE=pfadn(1:pfadi)//'River_Storage.out',STATUS='replace')
  if (f_river_storage) then
	WRITE (11,*) 'Output file for river water storage in m3 (with MAP IDs as in hymo.dat)'
	write(fmtstr,'(a,i0,a)')'(3a6,',subasin,'i14)'		!generate format string
	WRITE (11,fmtstr)' Year ', ' Day  ','  dt  ', (id_subbas_extern(i), i=1,subasin)
	Close (11)
   else
    close(11, status='delete') !delete any existing file, if no output is desired
  endif



  OPEN(11,FILE=pfadn(1:pfadi)//'River_Deposition.out',STATUS='replace')
  if (f_river_deposition) then
	WRITE (11,'(A)') 'Deposition of sediments in the riverbed in t/timestep (with MAP IDs as in hymo.dat)'
	write(fmtstr,'(a,i0,a)')'(3a6,',subasin,'i14)'		!generate format string
	WRITE (11,fmtstr)' Year ', ' Day  ','  dt  ', (id_subbas_extern(i), i=1,subasin)
	Close (11)
  else
    close(11, status='delete') !delete any existing file, if no output is desired
  endif

  OPEN(11,FILE=pfadn(1:pfadi)//'River_Degradation.out',STATUS='replace')
  if (f_river_degradation) then
	WRITE (11,*) 'Erosion of sediments in the riverbed in t/timestep (with MAP IDs as in hymo.dat)'
	write(fmtstr,'(a,i0,a)')'(3a6,',subasin,'i14)'		!generate format string
	WRITE (11,fmtstr)' Year ', ' Day  ','  dt  ', (id_subbas_extern(i), i=1,subasin)
	Close (11)
  else
    close(11, status='delete') !delete any existing file, if no output is desired
  endif


  OPEN(11,FILE=pfadn(1:pfadi)//'River_Flow_dailyaverage.out',STATUS='replace')
  if (f_river_flow_dailyaverage) then
	WRITE (11,*) 'Output files for river discharge q_out (m3/s) (with MAP IDs as in hymo.dat)'
	write(fmtstr,'(a,i0,a)')'(3a6,',subasin,'i14)'		!generate format string
	WRITE (11,fmtstr)' Year ', ' Day  ','  dt  ', (id_subbas_extern(i), i=1,subasin)
	Close (11)
  else
    close(11, status='delete') !delete any existing file, if no output is desired
  endif

  OPEN(11,FILE=pfadn(1:pfadi)//'River_Sediment_total_dailyaverage.out',STATUS='replace')
  if (f_river_sediment_total_dailyaverage) then
	WRITE (11,*) 'Output file for mean sediment flux in ton/h (with MAP IDs as in hymo.dat)'
	write(fmtstr,'(a,i0,a)')'(2a6,',subasin,'i14)'		!generate format string
	WRITE (11,fmtstr)' Year ', ' Day  ', (id_subbas_extern(i), i=1,subasin)
	Close (11)
  else
    close(11, status='delete') !delete any existing file, if no output is desired
  endif

END IF !routing mode=2
! ------------------------------------------------------------------------
IF (STATUS == 1) THEN
qsediment2_t=0.

! Initialisation for first year and day
 IF (t == tstart) THEN
    r_depth_cur(:)=0.
    r_qout(:,:)=0.
	r_qin(:,:)=0.

   if(river_transport.eq.2) then
      sediment_in(:,:)=0.
	  sediment_out(:,:)=0.

	  river_deposition(:,:)=0.
	  river_degradation(:,:)=0.
	  if (.not. doloadstate) then
	    riverbed_storage(:,:)=0.
        sed_storage(:,:)=0.
	  endif
	elseif(river_transport.eq.3) then
	  bedload(:,:) = 0.
	endif

   r_qin(1,:) = Q_spring(:)	!spring water, start of a river
   r_qin(2,:) = Q_spring(:) !spring water, start of a river
   r_qout(1,:)= Q_spring(:) !guestimation of the outflow discharge


 ENDIF


! CALL Reservoir Sedimentation and Management Modules
!George (status,upstream,h) instead (status,upstream)
 IF (doreservoir) CALL reservoir (status,upstream,h)

END IF

! ------------------------------------------------------------------------
IF (STATUS == 2) THEN

temp_water(:)=0. !for computing daily averages
temp_sediment(:)=0.

! Calculation of inflow and outflow in stream flow order
DO h=1,nt
  DO i=1,subasin

  !Water (m3/s) and sediment (ton/dt) fluxes from the hillslope module are added to river stretch
	r_qin(2,i) = water_subbasin_t(d,h,i) + Q_Spring(i)
	if(dosediment.and.river_transport.eq.2) then
		sediment_in(i, :) = sediment_subbasin_t(d,h,i,:)
    endif !(dosediment)
  enddo	!(i=1, subasin)

!George (initialization of the variable that calculates lateral water inflow into the reservoirs)
!*************************************************************************
  IF (doreservoir .and. dosediment) then
	    sed_qlateral(:,:)=0.
  ENDIF
!*************************************************************************

  DO i=1,subasin
   upstream=upbasin(i)  !internal code-ID for most upstream sub-basin
   downstream=downbasin(i) !internal code-ID for receiving sub-basin


   log_temp = .FALSE.
   if (do_pre_outflow(i)) then
        log_temp = .TRUE.		!Till: if water outflow from upstream subbasins is given
   end if

   if (log_temp) then		!Till: if water outflow from upstream subbasins is given
     r_qout(2,upstream)=water_subbasin_t(d,h,upstream)
   else
     call muskingum (upstream, h)								!normal routing !ii:neither flow nor r_area is used further
   end if

   log_temp = .FALSE.
   if (do_pre_outsed) then
        if (associated(corr_column_pre_subbas_outsed)) then   !Till: successive evaluation is necessary to adhere to Fortran standard (no short-circuit-evaluation guaranteed)
            if (corr_column_pre_subbas_outsed(i)>0) log_temp = .TRUE.		!Till: if water outsed from upstream subbasins is given and if outsed of subbasin is prespecified
        end if
    end if

   if (log_temp) then		!Till: if water outsed from upstream subbasins is given and if outsed of subbasin is prespecified
	do k=1,n_sed_class
		sediment_out(upstream,k)=sediment_subbasin_t(d,h,upstream,k)
	end do
   else																		!normal sediment routing
	if (dosediment) then
     if (river_transport.eq.2) call route_sediments(upstream)
     if(river_transport.eq.3) call bedload_formulae(upstream)
	endif
   end if !do outsed


!George Loop was changed in April 2007 after including lateral inflow into the dowstream reservoirs
!*************************************************************************************
!   IF (downstream /= 9999 .AND. downstream /= 999) THEN !George
!     r_qin(2,downstream)=r_qin(2,downstream) + r_qout(2,upstream) !George
!	 if(dosediment.and.river_transport.eq.2) then !George
!	   do k=1,n_sed_class !George
!		 sediment_in(downstream, k)=sediment_in(downstream,k) + sediment_out(upstream,k) !George
!	   enddo !George
!	 endif !George
!   END IF !George

   IF (doreservoir) THEN
!George calculation of the simulation timestep "step"
     step=(d-1)*nt+h
     IF (storcap(upstream) > 0. .and. t >= damyear(upstream)) THEN
!George (status,upstream,h) instead (status,upstream)
       CALL reservoir (status,upstream,h)
	 ENDIF
   ENDIF

   IF (doreservoir) THEN
     IF (latflow_res(upstream) == 0) THEN
       IF (storcap(upstream) > 0. .and. t >= damyear(upstream)) THEN
         IF (downstream /= 9999 .AND. downstream /= 999) THEN
!George res_qout(step,upstream) instead qout(d,upstream)
!write(*,*)step,id_subbas_extern(upstream),"case 1a"
           r_qin(2,downstream)=r_qin(2,downstream)+res_qout(step,upstream)
	       if(dosediment) then
	         do k=1,n_sed_class
	           sediment_in(downstream,k)=sediment_in(downstream,k)+res_sediment_out(upstream,k)
	         enddo
	       endif
         END IF
	   ELSE
         IF (downstream /= 9999 .AND. downstream /= 999) THEN !George
!write(*,*)step,id_subbas_extern(upstream),"case 1b"
           r_qin(2,downstream)=r_qin(2,downstream) + r_qout(2,upstream) !George
	       if(dosediment.and.river_transport.eq.2) then !George
	         do k=1,n_sed_class !George
		       sediment_in(downstream, k)=sediment_in(downstream,k) + sediment_out(upstream,k) !George
	         enddo !George
	       endif !George
         END IF !George
	   ENDIF
	 ELSE IF (latflow_res(upstream) == 1) THEN
       IF (storcap(upstream) > 0. .and. t >= damyear(upstream)) THEN
         IF (storcap(reservoir_down(upstream)) > 0. .and. t >= damyear(reservoir_down(upstream))) THEN
!write(*,*)step,id_subbas_extern(upstream),"case 2a"
           qlateral(step,reservoir_down(upstream))=qlateral(step,reservoir_down(upstream))+res_qout(step,upstream)
	       if(dosediment) then
	         do k=1,n_sed_class
	           sed_qlateral(reservoir_down(upstream),k)=sed_qlateral(reservoir_down(upstream),k)+res_sediment_out(upstream,k)
	         enddo
	       endif
		 ELSE
           IF (downstream /= 9999 .AND. downstream /= 999) THEN !George
!write(*,*)step,id_subbas_extern(upstream),"case 2b"
             r_qin(2,downstream)=r_qin(2,downstream) + res_qout(step,upstream) !George
	         if(dosediment.and.river_transport.eq.2) then !George
	           do k=1,n_sed_class !George
		         sediment_in(downstream, k)=sediment_in(downstream,k) + res_sediment_out(upstream,k) !George
	           enddo !George
	         endif !George
           END IF !George
		 ENDIF
	   ELSE
         IF (storcap(reservoir_down(upstream)) > 0. .and. t >= damyear(reservoir_down(upstream))) THEN
!write(*,*)step,id_subbas_extern(upstream),"case 3a"
           qlateral(step,reservoir_down(upstream))=qlateral(step,reservoir_down(upstream))+r_qout(2,upstream)
	       if(dosediment) then
	         do k=1,n_sed_class
	           sed_qlateral(reservoir_down(upstream),k)=sed_qlateral(reservoir_down(upstream),k)+sediment_out(upstream,k)
	         enddo
	       endif
		 ELSE
           IF (downstream /= 9999 .AND. downstream /= 999) THEN !George
!write(*,*)step,id_subbas_extern(upstream),"case 3b"
             r_qin(2,downstream)=r_qin(2,downstream) + r_qout(2,upstream) !George
	         if(dosediment.and.river_transport.eq.2) then !George
	           do k=1,n_sed_class !George
		         sediment_in(downstream, k)=sediment_in(downstream,k) + sediment_out(upstream,k) !George
	           enddo !George
	         endif !George
           END IF !George
		 ENDIF
	   ENDIF
	 ENDIF
   ELSE
     IF (downstream /= 9999 .AND. downstream /= 999) THEN !George
!write(*,*)step,id_subbas_extern(upstream),"case 4"
       r_qin(2,downstream)=r_qin(2,downstream) + r_qout(2,upstream) !George
	   if(dosediment.and.river_transport.eq.2) then !George
	     do k=1,n_sed_class !George
		   sediment_in(downstream, k)=sediment_in(downstream,k) + sediment_out(upstream,k) !George
	     enddo !George
	   endif !George
     END IF !George
   ENDIF ! doreservoir
!do k=1,n_sed_class
!dummy=dummy+sediment_subbasin_t(d,h,i,k)
!enddo
!write(*,*)d,id_subbas_extern(upstream),dummy,(sediment_out(upstream,k),k=1,n_sed_class)
!if (d==2)stop

   if (dosediment) then
	 do k=1,n_sed_class !George
	  IF (doreservoir) THEN
       sedinflow_g(step,upstream,k)=sediment_in(upstream,k)+sed_qlateral(upstream,k)
       IF (storcap(upstream) > 0. .and. t >= damyear(upstream)) THEN
         sedoutflow_g(step,upstream,k)=res_sediment_out(upstream,k)
	   ELSE
         sedoutflow_g(step,upstream,k)=sedinflow_g(step,upstream,k)
	   ENDIF
	  ENDIF
	 enddo
   endif
!*************************************************************************

!for daily averages
if (dohour) then
  temp_water(i) = temp_water(i) + r_qout(2,i)
  temp_sediment(i) = temp_sediment(i) + sum(sediment_out(i,1:n_sed_class))
endif




  END DO ! i=1,subasin


  if (f_river_flow) riverflow_t(d,h,:) = r_qout(2,:) !collect riverflow for later output

	r_sediment_storage=sum(sed_storage,dim=2)  !Till: sum up suspended sediment storage over all particle classes for all subbasins

! add up all sediment size classes to obtain total sediment mass
  do i = 1, subasin
     qsediment2_t(d,h,i) = qsediment2_t(d,h,i) + sum(sediment_out(i, 1:n_sed_class))
  enddo

! calculate total sediment concentration
   do i = 1, subasin
     if (r_qout(2,i).eq. 0.) then
        r_sediment_concentration(i) = 0.
     else
        r_sediment_concentration(i) = qsediment2_t(d,h,i)/(r_qout(2,i)*3.6*dt)
     endif
  enddo



if (river_transport.eq.2) then !ii: modify arrays to collect data of more timesteps (as e.g. qsediment2_t) to avoid file access in each single timestep (slow!)
  if (f_river_sediment_concentration) then
	OPEN(11,FILE=pfadn(1:pfadi)//'River_Sediment_Concentration.out',STATUS='old' ,POSITION='append'  )
	write(fmtstr,'(a,i0,a)')'(3i6,',subasin,'f14.3)'		!generate format string
	WRITE (11,fmtstr)t,d,h, (r_sediment_concentration(i),i=1,subasin)
	CLOSE (11)
  endif
elseif(river_transport.eq.3) then
   if(f_river_bedload) then
	OPEN(11,FILE=pfadn(1:pfadi)//'River_Bedload.out',STATUS='old' ,POSITION='append'  )
	write(fmtstr,'(a,i0,a)')'(3i6,',5*subasin,'f14.3)'		!generate format string
	WRITE (11,fmtstr)t,d,h, (bedload(i,1), bedload(i,2),bedload(i,3), bedload(i,4), bedload(i,5), i=1,subasin)
	CLOSE (11)
   endif
endif

  if(f_river_velocity) then
	OPEN(11,FILE=pfadn(1:pfadi)//'River_Velocity.out',STATUS='old' ,POSITION='append'  )
	write(fmtstr,'(a,i0,a)')'(3i6,',subasin,'f14.3)'		!generate format string
	WRITE (11,fmtstr)t, d,h, (velocity(i),i=1,subasin)
	CLOSE (11)
  endif

  if(f_river_storage) then
	OPEN(11,FILE=pfadn(1:pfadi)//'River_Storage.out',STATUS='old' ,POSITION='append'  )
	write(fmtstr,'(a,i0,a)')'(3i6,',subasin,'f14.3)'		!generate format string
	WRITE (11,fmtstr)t, d,h, (r_storage(i),i=1,subasin)
	CLOSE (11)
  endif

  if(f_river_sediment_storage) then
	river_sediment_storage_t(d,h,:)=sum(riverbed_storage(:,:), dim=2) !save current values in output array
  endif

  if(f_river_susp_sediment_storage) then
	river_susp_sediment_storage_t(d,h,:)=r_sediment_storage(:) !save current values in output array
  endif

  if(f_river_flowdepth) then
	OPEN(11,FILE=pfadn(1:pfadi)//'River_Flowdepth.out',STATUS='old' ,POSITION='append'  )
	write(fmtstr,'(a,i0,a)')'(3i6,',subasin,'f14.3)'		!generate format string
	WRITE (11,fmtstr)t, d,h, (r_depth_cur(i),i=1,subasin)
	CLOSE (11)
  endif

  if(f_river_deposition) then
	OPEN(11,FILE=pfadn(1:pfadi)//'River_Deposition.out',STATUS='old' ,POSITION='append'  )
	write(fmtstr,'(a,i0,a,a,a)')'(i0,a,i0,a,i0,',subasin,'(a,',fmt_str(maxval(river_deposition(1:subasin,1))),'))'		!generate format string
	WRITE (11,fmtstr)t, char(9),d,char(9), h, (char(9),river_deposition(i,1),i=1,subasin)
	CLOSE (11)
  endif

  if(f_river_degradation) then
	OPEN(11,FILE=pfadn(1:pfadi)//'River_Degradation.out',STATUS='old' ,POSITION='append'  )
	write(fmtstr,'(a,i0,a)')'(3i6,',subasin,'f14.3)'		!generate format string
	WRITE (11,fmtstr)t, d,h, (river_degradation(i,1),i=1,subasin)
	CLOSE (11)
  endif



 ! for output of daily averages
  if (dohour.and.h.eq.24) then
    if (f_river_flow_dailyaverage) then
		OPEN(11,FILE=pfadn(1:pfadi)//'River_Flow_dailyaverage.out',STATUS='old' ,POSITION='append')
		write(fmtstr,'(a,i0,a)')'(3i6,',subasin,'f14.3)'		!generate format string
		WRITE (11,fmtstr)t,d,h, ((temp_water(i)/24.),i=1,subasin)
		Close (11)
	endif

	if (f_river_sediment_total_dailyaverage) then
		OPEN(11,FILE=pfadn(1:pfadi)//'River_Sediment_total_dailyaverage.out',STATUS='old' ,POSITION='append')
		write(fmtstr,'(a,i0,a)')'(2i6,',2*subasin,'f14.3)'		!generate format string
		WRITE (11,fmtstr)t,d, (temp_sediment(i)/24.,i=1,subasin)
		Close (11)
	endif

    temp_water(:) = 0.
	temp_sediment(:) = 0.
  endif

! Update flows for next timestep
  DO i=1,subasin
    qout(d,i)= r_qout(2,i)

	r_qin(1,i) = r_qin(2,i)
	r_qout(1,i)= r_qout(2,i)
	r_qin(2,i)=0.
	r_qout(2,i)=0.

	sediment_in(i, 1:n_sed_class)=0.
  ENDDO


ENDDO	! end of day or hourly loop






END IF


! -----------------------------------------------------------------------
IF (STATUS == 3) THEN
  IF (doreservoir) THEN
    CALL  reservoir (status,upstream,h)
  END IF


! output of daily water and discharge in the river for entire year
	if (.not.dohour .and. (river_transport == 1)) then
		if (f_river_flow) then
			OPEN(11,FILE=pfadn(1:pfadi)//'River_Flow.out',STATUS='old' ,POSITION='append'  )
			write(fmtstr,'(a,i0,a)')'(2i6,',subasin,'f14.3)'		!generate format string
			do j=1, dayyear
				WRITE (11,fmtstr)t, j, (qout(j,i),i=1,subasin)
			enddo
			CLOSE (11)
		endif

    endif

END IF




RETURN
END SUBROUTINE routing_new

