!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module common_h
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    save
    ! logicals for inclusion of modules in calculation
    CHARACTER (LEN=80) ::rev_string1,rev_string2   !Till: revision information string
    LOGICAL :: doacud
    LOGICAL :: doreservoir
    LOGICAL :: dolattc
    LOGICAL :: doalllattc
    LOGICAL :: dolatsc
    LOGICAL :: dolatscsub
    LOGICAL :: dotrans
    LOGICAL :: docellprec
    LOGICAL :: dohour
    LOGICAL :: doscale
    LOGICAL :: domuncell
    LOGICAL :: doreserv
    LOGICAL, allocatable :: do_pre_outflow(:)   !Till: consider pre-specified outflow time series from subbasins
    LOGICAL :: do_pre_outsed=.TRUE.   !Till: consider pre-specified sediment outflow time series from subbasins

    INTEGER :: ezgt
    INTEGER :: zahlt
    INTEGER :: nameti
    INTEGER :: nmunt
    INTEGER :: nroutt
    INTEGER :: batch
    INTEGER :: scenario
    INTEGER :: krig
    REAL :: sensfactor
    CHARACTER (LEN=10) :: namet
    CHARACTER (LEN=10) :: caset
    CHARACTER (LEN=160) :: pfadn
    CHARACTER (LEN=160) :: pfadp
    INTEGER :: pfadi
    INTEGER :: pfadj
    INTEGER :: ncaset
    INTEGER :: nezgt
    INTEGER :: pyear
    INTEGER :: dt               !time step in [hours]
    LOGICAL :: dosediment         !do sediment calculation for hillslope, river and reservoir
    INTEGER :: n_sed_class         !number of particles classes
    INTEGER :: river_transport      !type of river sediment transport
    INTEGER :: reservoir_transport   !type of reservoir sediment transport
    INTEGER :: nt               !number of timesteps per day (computed as 24/dt)
    INTEGER :: dosnow               !snow module to be used: 0 => NO; 1 => YES
    INTEGER :: julian_day ! julian day

    real, allocatable :: upper_limit(:)         !upper limits of particle size class intervalls (mm)
    real, allocatable :: particle_classes(:)   !upper limits of particle size classes [mm]

    !flags for selecting which output files will be created
    LOGICAL :: f_daily_actetranspiration,f_daily_potetranspiration, f_daily_qhorton,f_daily_qin_m3s,f_daily_qout_m3s,f_daily_rain,f_daily_runoff
    LOGICAL :: f_daily_sediment_production,f_daily_subsurface_runoff,f_daily_theta,f_daily_total_overlandflow,f_daily_water_subbasin,f_routing_response,f_sediment_production,f_water_subbasin,f_deep_gw_recharge,f_deep_gw_discharge,f_daily_gw_loss,f_tc_theta
    LOGICAL :: f_tc_surfflow, f_tc_sedout, f_lu_sedout
    LOGICAL :: f_river_degradation, f_river_deposition, f_river_flow, f_river_flow_dailyaverage, f_river_flowdepth, f_river_sediment_concentration, f_river_sediment_total, f_river_sediment_total_dailyaverage, f_river_storage, f_river_sediment_storage
    LOGICAL :: f_river_susp_sediment_storage, f_river_velocity, f_river_bedload, f_river_infiltration, f_actetranspiration,f_qhorton,f_subsurface_runoff,f_total_overlandflow,f_gw_discharge,f_potetranspiration,f_gw_loss,f_gw_recharge
    LOGICAL :: f_res_watbal,f_res_vollost,f_res_cav,f_res_hydraul,f_res_bedchange,f_res_sedbal,f_res_longitudunal,f_res_sedcomposition
    LOGICAL :: f_lake_inflow_r,f_lake_outflow_r,f_lake_retention_r,f_lake_volume_r,f_lake_sedinflow_r,f_lake_sedoutflow_r,f_lake_sedretention_r,f_lake_sedimentation_r
    LOGICAL :: f_lake_watbal,f_lake_sedbal,f_lake_inflow,f_lake_outflow,f_lake_volume,f_lake_retention,f_lake_vollost,f_lake_sedinflow,f_lake_sedoutflow,f_lake_sizedistoutflow
    LOGICAL :: f_snowEnergyCont, f_snowWaterEquiv, f_snowAlbedo, f_snowCover, f_snowTemp, f_surfTemp, f_liquFrac, f_fluxPrec, f_fluxSubl, f_fluxFlow, f_fluxNetS, f_fluxNetL, f_fluxSoil, f_fluxSens, f_stoiPrec, f_stoiSubl, f_stoiFlow, f_rateAlbe, f_precipMod, f_cloudFrac,f_radiMod,f_temperaMod,f_rel_elevation
    LOGICAL :: do_rad_corr=.FALSE.   ! modification of radiation with aspect and slope
    LOGICAL :: do_alt_corr=.FALSE.   ! modification of temperature with altitude of LU


    LOGICAL :: doloadstate=.FALSE.         !load initial values before model run
    LOGICAL :: dosavestate=.FALSE.         !save state of model after execution
    LOGICAL :: append_output=.FALSE.       !append model output to potentially existing file
    LOGICAL :: save_states_yearly=.TRUE.       !save model state at the end of each simulation year


    REAL   :: default_rel_sat=1.0      !default relative saturation (initial value for all horizons, if not specified otherwise)
    REAL   :: default_gw_storage=0.0   !default ground water storage (to be assumed for all non-specified LUs) [mm]



    logical,parameter :: domean=.FALSE.   ! calculate daily ETP from mean daily values (this flag is dominant over donight)
    logical,parameter :: donight=.TRUE.      !if (domean==0):calculate daily ETP as sum of ETP for day and night (donight=1) or only day
    real :: daily_delta_temp =0. !daily temperature amplitude (Tmin=Tmean-daily_delta_temp; Tmax=Tmean+daily_delta_temp;)
                                !ii: This should better be a parameter or for hourly data computed
    REAL,parameter :: hours_of_daylight=12.0 !number of hours with sunlight per day !ii: improve this with radiation correction
    REAL,parameter :: pi=3.141593 !well, pi

    !REAL :: debugcheck(30,2)=0. !just for debugging, remove
end module common_h
