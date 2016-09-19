! This code runs the Snow17, Sacramento and UH routing model for multiple areas within a basin
!   and makes an areal average of the output
!
! Orig:  2014 -- Andy Newman (NCAR) chopped this out of larger calibration code set  
! Modifications: 
!   AWW-20160107:  
!     adapted to run with input pcp, tmax, tmin, and external pet forcings
!     cleaned up and added documention
! 
!   AWW-20160121:  
!     made to run in loop so that multiple outputs could be combined
!
!   Known TODOs:  - convert old internal PET approach into option
!                 - calc sim_length using date math instead of forcing read
!                 - make params variable into 2-d arrays (or otherwise simplify i/o

program multi_driver

  ! specify modules needed
  use nrtype
  use def_namelists
  use constants, only:   sec_hour,sec_day
  use interfaces, only:  read_namelist, calc_pet_pt, sfc_pressure, &
                         read_areal_forcing,read_streamflow, get_sim_length, &
			 julian_day,write_snow17_state,write_sac_state, &
			 read_sac_state,read_snow17_state,read_uh_state, &
			 write_uh_state, date_diff_ndays
  implicit none

  ! local variables
  character(len=2000)		:: arg  !command line arg for namelist file
  character(len=2000)		:: namelist_name  !command line arg for namelist file
  character(len=2000)		:: control_filename          ! AWW command line arg for control_filename
  character(len=2000)		:: combined_output_filename  ! AWW given in control file

  integer      :: n_areas, na      ! AWW number of areas to simulate, and index to use
  integer(I4B) :: area_sqkm, total_area_sqkm  ! AWW needed for combining outputs
  integer(I4B) :: i,ntau,k,m
  integer(I4B) :: sim_length   ! length of simulation
  real(sp)     :: dtuh	       ! for unit hydrograph

  ! single precision sac-sma state variables
  real(sp)		:: uztwc_sp
  real(sp)		:: uzfwc_sp
  real(sp)		:: lztwc_sp
  real(sp)		:: lzfsc_sp
  real(sp)		:: lzfpc_sp
  real(sp)		:: adimc_sp
  real(sp)		:: pet_sp
  real(sp)		:: tair_sp
  real(sp)		:: precip_sp

  ! snow-17 carry over variables
  real(dp)                :: pa       ! snow-17 surface pressure
  real(sp)                :: tprev    ! carry over variable
  real(sp),dimension(19)  :: cs       ! carry over variable array
  real(sp),dimension(:),allocatable    :: tprev_states    ! for state file write
  real(sp),dimension(:,:),allocatable  :: cs_states       ! for state file write

  ! unit hydrograph
  real(sp),dimension(50)       :: unit_hydro ! in time steps ... this dim sets UH_LENGTH: 
                                               ! could cause truncation if UH is very long
                                               ! needs to match setting in duamel

  integer(I4B)		       :: uh_length
  integer(I4B)                 :: TOC_length ! DUAMEL.f new return variable -- A.Wood Feb 2016

  ! arrays for using state restart with unit hydrograph
  real(sp),dimension(:),allocatable	:: prior_tci,tmp_tci ! AWW added

  ! ==== ALLOCATABLE VARIABLES ====

  ! sac-sma state variables
  real(dp),dimension(:),allocatable     :: uztwc_dp, uztwc_comb  ! AWW added combined vars
  real(dp),dimension(:),allocatable     :: uzfwc_dp, uzfwc_comb
  real(dp),dimension(:),allocatable     :: lztwc_dp, lztwc_comb
  real(dp),dimension(:),allocatable     :: lzfsc_dp, lzfsc_comb
  real(dp),dimension(:),allocatable     :: lzfpc_dp, lzfpc_comb
  real(dp),dimension(:),allocatable     :: adimc_dp, adimc_comb

  ! sac-sma output variables and routed flow
  real(sp), dimension(:),allocatable	:: qs,qg,eta,tci,route_tci
  real(sp), dimension(:),allocatable	:: eta_comb,tci_comb,route_tci_comb ! AWW combined vars
  real(sp), dimension(:),allocatable	:: route_tci_cfs ! AWW comb. var, convert mm/d to cfs while data are handy

  ! snow-17 output variables  single precision
  real(sp), dimension(:),allocatable    :: snowh, sneqv, snow !output variables for snow-17
  real(sp), dimension(:),allocatable    :: sneqv_comb ! AWW ditto, combined vars

  ! date variables
  integer, dimension(:),allocatable :: year,month,day,hour,jday

  ! observed streamflow
  real(dp),dimension(:),allocatable :: obs_flow
  real(sp),dimension(:),allocatable :: raim
  real(sp),dimension(:),allocatable :: raim_comb  ! AWW combined vars

  ! atmospheric forcing variables
  real(dp), dimension(:),allocatable    :: tmin,tmax,vpd,dayl,swdown,precip
  real(dp), dimension(:),allocatable    :: precip_comb, precip_scf_comb ! AWW combined vars
  ! derived forcing variables
  real(dp), dimension(:),allocatable :: pet,tair
  real(dp), dimension(:),allocatable :: pet_comb,tair_comb  ! AWW combined vars

  ! =======  CODE starts below =====================================================================

  ! get control file filename as argument
  i = 0
  do
    call get_command_argument(i,arg)
    if(i .eq. 1) control_filename=arg   ! first argument has i=1
    if(LEN_TRIM(arg) == 0) EXIT
    i = i + 1
  end do

  ! read control file, which should have the # of units to simulate, followed by each of their namelists
  open(unit=25,FILE=trim(control_filename),FORM='formatted',status='old')
  read(25,*) n_areas, combined_output_filename
  if(n_areas < 1) then
    print *, "EXITING: Number of simulation areas (n_areas) < 1 =",n_areas
    stop
  endif 

  uh_length = size(unit_hydro,1)   ! length of unit hydrograph

  ! ========================= AREA LOOP ========================================================
  !   loop through the simulation areas, running the lump model code and averaging the output

  print*, '--------------------------------------------------------------'
  do na=1,n_areas

    ! get namelist for current area
    read(25,*) namelist_name
    print*, 'Running area',na,'out of',n_areas,'described in ',trim(namelist_name)

    ! read namelist file for current simulation area
    call read_namelist(namelist_name)
  
    ! read parameter files
    call read_sac_params(sac_param_file)
    call read_snow17_params(snow17_param_file)
    call read_uhp_params(uhp_param_file)

    ! --- allocate variables (before simulating first area) ---

    if(na .eq. 1) then
      ! get sim length to use in allocating variables (AWW new routine)
      call date_diff_ndays(start_year,start_month,start_day,end_year,end_month,end_day,sim_length)
      print*, 'sim_length =',sim_length
     
      !forcing variables
      allocate(year(sim_length))
      allocate(month(sim_length))
      allocate(day(sim_length))
      allocate(jday(sim_length))
      allocate(hour(sim_length))
      allocate(tmax(sim_length))
      allocate(tmin(sim_length))
      allocate(vpd(sim_length))
      allocate(dayl(sim_length))
      allocate(swdown(sim_length))
      allocate(precip(sim_length))
      allocate(pet(sim_length))
      allocate(tair(sim_length))
  
      !observations
      allocate(obs_flow(sim_length))
  
      !sac-sma state variables
      allocate(uztwc_dp(sim_length))
      allocate(uzfwc_dp(sim_length))
      allocate(lztwc_dp(sim_length))
      allocate(lzfsc_dp(sim_length))
      allocate(lzfpc_dp(sim_length))
      allocate(adimc_dp(sim_length))
  
      !sac-sma output variables
      allocate(qg(sim_length))
      allocate(qs(sim_length))
      allocate(eta(sim_length))
      allocate(tci(sim_length))
  
      ! snow-17 output variables
      allocate(snowh(sim_length))
      allocate(sneqv(sim_length))
      allocate(snow(sim_length))
      allocate(raim(sim_length))
      allocate(cs_states(19,sim_length)) ! new for state write
      allocate(tprev_states(sim_length)) ! ditto

      ! UH routing variables
      allocate(tmp_tci(sim_length+uh_length))   ! input -- may have to accomodate spinup routing
      allocate(route_tci(sim_length+uh_length)) ! output -- may have to accomodate spinup routing
      allocate(prior_tci(uh_length))            ! related (for state file read)

    endif  ! end if case for allocating only when running the first simulation area

    ! read forcing data
    call read_areal_forcing(year,month,day,hour,tmin,tmax,precip,pet)  ! hour not needed
    print*, '  start:',year(1),month(1),day(1)
    print*, '    end:',year(sim_length),month(sim_length),day(sim_length) 
    ! BUG:  for some reason only  works if 2nd print appears, otherwise year month day vars get corrupted
    !       implies a memory leak somewhere (haven't found)

    ! read streamflow (on first area only), calculate julian day
    if(na == 1) then
       if (stream_name == '-9999' .or. stream_name == '-9999.0' .or. stream_name == '-9' &
       .or. stream_name == '-99' .or. stream_name == '-999' .or. stream_name == '-9.0' &
       .or. stream_name == '-99.0' .or. stream_name == '-999.0') then
        obs_flow = -9999.0
      else
        call read_streamflow(obs_flow)
      endif

      !get day of year array setup
      call julian_day(year,month,day,jday)

    endif
  
    tair = (tmax+tmin)/2.0_dp
  
    ! compute pet
    !  call calc_pet_pt(jday,tmax,tmin,tair,vpd,swdown,dayl,pet)
    !  AWW -- already have PET in input, for this version
  
    ! ================== run models! ===================
  
    ! get sfc_pressure (pa is estimate by subroutine, needed by snow17 call)
    call sfc_pressure(elev,pa)
  
    ! set single precision sac state variables to initial values
    if(restart_run .eq. 0) then
      ! we are not restarting from a state file
      uztwc_sp = in_uztwc
      uzfwc_sp = in_uzfwc
      lztwc_sp = in_lztwc
      lzfsc_sp = in_lzfsc
      lzfpc_sp = in_lzfpc
      adimc_sp = in_adimc
      cs(1)    = in_swe   ! AWW: just initialize first/main component of SWE (model 'WE')
      cs(2:19) = 0        !      set the rest to zero
      tprev    = 0        ! AWW ADDED

    elseif(restart_run .gt. 0) then
      ! we ARE restarting from a state file
      ! read in external state files and overwrites namelist state variables 

      ! states are pre-processed to just hold one records values (in a column) from the output state files
      call read_snow17_state(cs,tprev)
      call read_sac_state(uztwc_sp,uzfwc_sp,lztwc_sp,lzfsc_sp,lzfpc_sp,adimc_sp)
      call read_uh_state(prior_tci,uh_length)
    endif
  
    ! =============== START SIMULATION TIME LOOP =====================================
    do i = 1,sim_length,1
  
      !set single precision inputs
      tair_sp   = real(tair(i),kind(sp))
      precip_sp = real(precip(i),kind(sp))
      pet_sp    = real(pet(i),kind(sp))
   
      call exsnow19(int(dt),int(dt/sec_hour),day(i),month(i),year(i),&
  	!SNOW17 INPUT AND OUTPUT VARIABLES
  			  precip_sp,tair_sp,raim(i),sneqv(i),snow(i),snowh(i),&
  	!SNOW17 PARAMETERS
          !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
          !			    alat,a(1),a(2),a(3),a(4),a(5),a(7),a(8),a(9),&
          !			    a(6),a(10),a(11),elev,pa,adc(1),&
  			  real(lat,kind(sp)),scf,mfmax,mfmin,uadj,si,nmf,tipm,mbase,&
  			  pxtemp,plwhc,daygm,real(elev,kind(sp)),real(pa,kind(sp)),adc,&
  	!SNOW17 CARRYOVER VARIABLES
  			  cs,tprev) 
  
      call exsac(1,real(dt),raim(i),tair_sp,pet_sp,&
  	!SAC PARAMETERS
          !UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC, &
          !REXP,LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE, &
          !SIDE,RSERV, &
          !a(12),a(13),a(18),a(23),a(17),a(25),a(21),a(22),a(14),a(16),a(15),a(20),a(19),a(24),a(26),a(27)
          !			a(12),a(13),a(18),a(23),a(17),a(25),a(21),&
          !			a(22),a(14),a(16),a(15),a(20),a(19),a(24),&
          !			a(26),a(27),&
  		      uztwm,uzfwm,uzk,pctim,adimp,riva,zperc, &
  		      rexp,lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree,&
  		      side,rserv, &
  	!SAC State variables
  			uztwc_sp,uzfwc_sp,lztwc_sp,lzfsc_sp,lzfpc_sp,adimc_sp,&
  	!SAC OUTPUTS
  			qs(i),qg(i),tci(i),eta(i))
    
      ! place state variables in output arrays
      uztwc_dp(i) = real(uztwc_sp,kind(dp))
      uzfwc_dp(i) = real(uzfwc_sp,kind(dp))
      lztwc_dp(i) = real(lztwc_sp,kind(dp))
      lzfsc_dp(i) = real(lzfsc_sp,kind(dp))
      lzfpc_dp(i) = real(lzfpc_sp,kind(dp))
      adimc_dp(i) = real(adimc_sp,kind(dp))

      ! store other single timestep run outputs for possible state write
      if(write_restart .gt. 0) then
        cs_states(:,i)  = cs
        tprev_states(i) = tprev
        ! print*,'tprev: ',i,day(i),month(i),year(i),tprev,cs(1)  AWW debugging
      endif  

    enddo  
    ! ============ end simulation time loop ====================
  
    ! ============ now route full runoff timeseries using UH =========================
    dtuh = real(dt/sec_day)
    if (unit_shape .le. 0.0 .and. unit_scale .le. 0.0) THEN
      k = 0
      m = 1
    else
      k = 1      ! 
      m = 1000   ! max UH length
    end if
    ntau = 0
  
    ! if routing is non-trivial, call unit hydrograph routine
    if(unit_shape .gt. 0.0) then
      ! call DUAMEL(tci,1,unit_hydro,unit_shape,unit_scale,dtuh,sim_length+uh_length,m,route_tci,k,ntau)

      if(restart_run .gt. 0) then
        ! prepend TCI from prior simulation to TCI, then rout, and reset routed flow to correct period
        tmp_tci(1:uh_length) = prior_tci  
        tmp_tci(uh_length+1:sim_length+uh_length) = tci(1:sim_length)
        call DUAMEL(tmp_tci,1,unit_hydro,unit_shape,unit_scale,dtuh,sim_length+uh_length,m,route_tci,k,ntau,TOC_length)!AWW          
        route_tci(1:sim_length) = route_tci(uh_length+1:sim_length+uh_length)  ! reset route_tci to correct time period
      else
        call DUAMEL(tci,1,unit_hydro,unit_shape,unit_scale,dtuh,sim_length+uh_length,m,route_tci,k,ntau,TOC_length)!AWW
      endif

      !print*, 'TOC_length', TOC_length, 'days'
      if(TOC_length > uh_length) then
        print*, ' '
        print*, 'WARNING:  TOC for simulation is greater than the routing UH_LENGTH' 
        print*, 'TOC_length =',TOC_length,' and uh_length =',uh_length
        print*, '    This is controlled by the dimension of unit_hydro'
        print*, '    It could cause flow initialization errors in a restart run'
        print*, '    Check UH params to assess length of UH'
        print*, 'CONTINUING nonetheless...'
        print*, ' '
      endif 

    endif  ! end if clause for whether to route at all


    ! ==== WRITE output for current area simulation ====

    open(unit=45,FILE=trim(model_out),FORM='formatted',status='replace')    ! output file open
  
    ! print header first
    !write(45,*) 'year mo dy hr tair pcp pcp*scf swe raim pet eta uztwc uzfwc lztwc lzfsc &
    !             &lzfpc adimc sim_runoff sim_flow_mmd obs_flow'
    write(45,'(A)') 'year mo dy hr tair pcp pcp*scf swe raim pet eta uztwc uzfwc lztwc lzfsc lzfpc adimc sim_runoff sim_flow_mmd obs_flow'
  
    31 FORMAT(I4.4, 3(1x,I2.2),7(F8.2),6(F8.2),3(F9.2),2(F10.3),2(F9.1))  ! AWW: goes with update
    32 FORMAT(I4.4, 3(1x,I2.2),7(F8.2),6(F8.2),2(F9.2),2(F10.3),2(F9.1))  ! AWW: one fewer field (routed tci)

    do i = 1,sim_length
      if(unit_shape .gt. 0) then
        write(45,31) year(i),month(i),day(i),hour(i),tair(i),precip(i),precip(i)*scf,&
                       sneqv(i)*1000.,raim(i),pet(i),eta(i),uztwc_dp(i),uzfwc_dp(i),&
                       lztwc_dp(i),lzfsc_dp(i),lzfpc_dp(i),adimc_dp(i),&
                       tci(i),route_tci(i),obs_flow(i)
      else
        ! AWW originally this was the same as above, but I think the routed flow needed removing
        write(45,32) year(i),month(i),day(i),hour(i)+12,sneqv(i)*1000.,precip(i),precip(i)*scf,raim(i),&
                          pet(i),eta(i),tair(i),uztwc_dp(i),uzfwc_dp(i),lztwc_dp(i),lzfsc_dp(i),lzfpc_dp(i),&
                          adimc_dp(i),tci(i),obs_flow(i)
      endif
    enddo
    close(unit=45)

    ! === write out STATE FILES for snow17, sac and uh if needed ===
    if(write_restart .gt. 0) then
      call write_snow17_state(year,month,day,hour,cs_states,tprev_states,sim_length)
      call write_sac_state(year,month,day,hour,uztwc_dp,uzfwc_dp,lztwc_dp,lzfsc_dp,lzfpc_dp,adimc_dp,sim_length)
      call write_uh_state(year,month,day,hour,tci,sim_length,uh_length)
    endif

    ! ==== AWW store/add single simulation area timeseries ====
    area_sqkm = area_basin / 1000 / 1000  ! convert to reduce size of multiplied vars

    if(na .eq. 1) then
      ! allocate combination output variables before first use
      allocate(tair_comb(sim_length))
      allocate(precip_comb(sim_length))
      allocate(precip_scf_comb(sim_length))
      allocate(sneqv_comb(sim_length))
      allocate(raim_comb(sim_length))
      allocate(pet_comb(sim_length))
      allocate(eta_comb(sim_length))
      allocate(uztwc_comb(sim_length))
      allocate(uzfwc_comb(sim_length))
      allocate(lztwc_comb(sim_length))
      allocate(lzfsc_comb(sim_length))
      allocate(lzfpc_comb(sim_length))
      allocate(adimc_comb(sim_length))
      allocate(tci_comb(sim_length+uh_length))
      allocate(route_tci_comb(sim_length+uh_length))
      allocate(route_tci_cfs(sim_length+uh_length))

      ! store current area variable multiplied by area (in sq-m, read from forcings)
      tair_comb       = tair * area_sqkm   
      precip_comb     = precip * area_sqkm
      precip_scf_comb = precip * scf * area_sqkm
      sneqv_comb      = sneqv * area_sqkm 
      raim_comb       = raim * area_sqkm
      pet_comb        = pet * area_sqkm
      eta_comb        = eta * area_sqkm
      uztwc_comb      = uztwc_dp * area_sqkm 
      uzfwc_comb      = uzfwc_dp * area_sqkm 
      lztwc_comb      = lztwc_dp * area_sqkm 
      lzfsc_comb      = lzfsc_dp * area_sqkm 
      lzfpc_comb      = lzfpc_dp * area_sqkm 
      adimc_comb      = adimc_dp * area_sqkm 
      tci_comb        = tci * area_sqkm
      route_tci_comb  = route_tci * area_sqkm
      total_area_sqkm = area_sqkm  ! initialize on first area
    else
      ! add to summary variables already started
      !    AWW 'area_basin' from 'use def_namelists' statement (in m-sq)
      tair_comb       = tair_comb + tair * area_sqkm
      precip_comb     = precip_comb + precip * area_sqkm
      precip_scf_comb = precip_scf_comb + precip * scf * area_sqkm
      sneqv_comb      = sneqv_comb + sneqv * area_sqkm 
      raim_comb       = raim_comb + raim * area_sqkm
      pet_comb        = pet_comb + pet * area_sqkm
      eta_comb        = eta_comb + eta * area_sqkm
      uztwc_comb      = uztwc_comb + uztwc_dp * area_sqkm 
      uzfwc_comb      = uzfwc_comb + uzfwc_dp * area_sqkm 
      lztwc_comb      = lztwc_comb + lztwc_dp * area_sqkm 
      lzfsc_comb      = lzfsc_comb + lzfsc_dp * area_sqkm 
      lzfpc_comb      = lzfpc_comb + lzfpc_dp * area_sqkm 
      adimc_comb      = adimc_comb + adimc_dp * area_sqkm 
      tci_comb        = tci_comb + tci * area_sqkm
      route_tci_comb  = route_tci_comb + route_tci * area_sqkm
      total_area_sqkm = total_area_sqkm + area_sqkm
    endif
  end do
  close(25)  ! close control file

  ! ========== END of simulation areas loop   ====================

  print*, '--------------------------------------------------------------'
  print*, 'Combining outputs and writing ', trim(combined_output_filename)

  ! take averages
  tair_comb       = tair_comb / total_area_sqkm
  precip_comb     = precip_comb / total_area_sqkm
  precip_scf_comb = precip_scf_comb / total_area_sqkm
  sneqv_comb      = sneqv_comb / total_area_sqkm
  raim_comb       = raim_comb / total_area_sqkm
  pet_comb        = pet_comb / total_area_sqkm
  eta_comb        = eta_comb / total_area_sqkm
  uztwc_comb      = uztwc_comb / total_area_sqkm
  uzfwc_comb      = uzfwc_comb / total_area_sqkm
  lztwc_comb      = lztwc_comb / total_area_sqkm
  lzfsc_comb      = lzfsc_comb / total_area_sqkm
  lzfpc_comb      = lzfpc_comb / total_area_sqkm
  adimc_comb      = adimc_comb / total_area_sqkm
  tci_comb        = tci_comb / total_area_sqkm
  route_tci_cfs   = route_tci_comb / 1000 * 3.28084 * (1000*3.28084)**2 / (24*3600)  ! mmd*area_sqkm to cfs
  route_tci_comb  = route_tci_comb / total_area_sqkm

  ! -- write out combined file that is similar to each area file, but add flow variable in CFS units

  ! combined output file open
  open(unit=125,FILE=trim(combined_output_filename),FORM='formatted',status='replace')
  
  ! print header first
  if(unit_shape .gt. 0) then
    write(125,'(A)') 'year mo dy hr tair pcp pcp*scf swe raim pet eta uztwc uzfwc lztwc lzfsc lzfpc adimc sim_runoff sim_flow_mmd sim_flow_cfs obs_flow'
  else  
    ! doesn't have routing
    write(125,'(A)') 'year mo dy hr tair pcp pcp*scf swe raim pet eta uztwc uzfwc lztwc lzfsc lzfpc adimc sim_runoff obs_flow'
  endif
  
  33 FORMAT(I4.4, 3(1x,I2.2),3(F8.2),F9.2,F8.2,2(F8.2),6(F9.2),2(F10.3),F12.2,F9.1)  ! AWW: 1 more field (cfs) than orig
  34 FORMAT(I4.4, 3(1x,I2.2),3(F8.2),F9.2,F8.2,2(F8.2),4(F9.2),2(F10.3),2(F9.1))  ! AWW: two fewer fields (not routed)

  do i = 1,sim_length
    if(unit_shape .gt. 0) then
      write(125,33) year(i),month(i),day(i),hour(i),tair_comb(i),precip_comb(i),precip_scf_comb(i),&
                       sneqv_comb(i)*1000.,raim_comb(i),pet_comb(i),eta_comb(i),uztwc_comb(i),uzfwc_comb(i),&
                       lztwc_comb(i),lzfsc_comb(i),lzfpc_comb(i),adimc_comb(i),&
                       tci_comb(i),route_tci_comb(i),route_tci_cfs(i), obs_flow(i)
    else
      ! doesn't have routing
      write(125,34) year(i),month(i),day(i),hour(i),tair_comb(i),precip_comb(i),precip_scf_comb(i),&
                       sneqv_comb(i)*1000.,raim_comb(i),pet_comb(i),eta_comb(i),uztwc_comb(i),uzfwc_comb(i),&
                       lztwc_comb(i),lzfsc_comb(i),lzfpc_comb(i),adimc_comb(i),&
                       tci_comb(i),obs_flow(i)
    endif
  enddo
  close(unit=125)

  print*, 'DONE!'
  print*, '--------------------------------------------------------------'

end program
