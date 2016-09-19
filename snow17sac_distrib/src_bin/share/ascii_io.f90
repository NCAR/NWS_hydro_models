subroutine write_snow17_state(year,month,day,hour,cs,tprev,sim_length)
  use nrtype
  use def_namelists, only: snow17_state_out_file
  implicit none

  !input variables
  integer(I4B),dimension(:),intent(in)	:: year
  integer(I4B),dimension(:),intent(in)	:: month
  integer(I4B),dimension(:),intent(in)	:: day
  integer(I4B),dimension(:),intent(in)	:: hour
  real(sp),dimension(:,:),intent(in)	:: cs	    ! carry over array
  real(sp),dimension(:),intent(in)	:: tprev    ! carry over variable
  integer(I4B),intent(in)               :: sim_length   ! length of simulation

  !local variables
  integer(I4B)	:: i

  open(unit=95,FILE=trim(snow17_state_out_file),FORM='formatted',status='replace')
  print*, 'Writing snow state file: ', trim(snow17_state_out_file)

  41 FORMAT(I4.4, 3(I2.2), 20(F20.12))  ! big enough to separate fields
  do i = 1,sim_length
    ! print*, 'tprev = ',tprev(i)  AWW debugging

    write(95,41) year(i),month(i),day(i),hour(i),cs(:,i),tprev(i)
  enddo

  close(unit=95)

  return
end subroutine write_snow17_state

! ccccccccccccccccccccccccccccccc

subroutine write_sac_state(year,month,day,hour,uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,sim_length)
  use nrtype
  use def_namelists, only: sac_state_out_file
  implicit none

  !input variables
  integer(I4B),dimension(:),intent(in)	:: year
  integer(I4B),dimension(:),intent(in)	:: month
  integer(I4B),dimension(:),intent(in)	:: day
  integer(I4B),dimension(:),intent(in)	:: hour
  real(dp),dimension(:),intent(in)	:: uztwc	!state variable
  real(dp),dimension(:),intent(in)	:: uzfwc	!state variable
  real(dp),dimension(:),intent(in)	:: lztwc        !state variable
  real(dp),dimension(:),intent(in)	:: lzfsc	!state variable
  real(dp),dimension(:),intent(in)	:: lzfpc	!state variable
  real(dp),dimension(:),intent(in)	:: adimc	!state variable
  integer(I4B),intent(in)               :: sim_length   ! length of simulation

  !local variables
  integer(I4B)	:: i

  42 FORMAT(I4.4, 3(I2.2), 6(F20.12))  ! big enough to separate fields
  open(unit=95,FILE=trim(sac_state_out_file),FORM='formatted',status='replace')
  print*, 'Writing sac state file: ', trim(sac_state_out_file)

  do i = 1,sim_length
    write(95,42) year(i),month(i),day(i),hour(i),uztwc(i),uzfwc(i),&
                       lztwc(i),lzfsc(i),lzfpc(i),adimc(i)
  enddo

  close(unit=95)

  return
end subroutine write_sac_state

! cccccccccccccccccccccccccccccccccccccccccc

subroutine write_uh_state(year,month,day,hour,tci,sim_length,uh_length)
  ! A.Wood, 2016 -- this routine writes out TCI (total channel input) for the 
  !   UH_LENGTH period preceding the first timestep of the simulation
  !   When read in, it is concatenated with the simulation TCI to initialize 
  !   the routing model
  use nrtype
  use def_namelists, only: uh_state_out_file
  implicit none

  !input variables
  integer(I4B),dimension(:),intent(in)	:: year
  integer(I4B),dimension(:),intent(in)	:: month
  integer(I4B),dimension(:),intent(in)	:: day
  integer(I4B),dimension(:),intent(in)	:: hour
  real(sp), dimension(:), intent(in) 	:: tci
  integer(I4B), intent(in)		:: sim_length
  integer(I4B), intent(in)		:: uh_length

  !local variables
  integer(I4B)	:: i
  real(sp),allocatable,dimension(:)	:: out_tci  ! holds tci except for 1st uh_length records

  ! pad uh_length-1 period prior to tci timeseries with zeros (since no other data)
  allocate(out_tci(uh_length-1+sim_length))
  out_tci(1:uh_length-1) = 0.0
  out_tci(uh_length:sim_length+uh_length-1) = tci

  44 FORMAT(I4.4, 3(I2.2), 1000(F20.12))  ! big enough to separate fields
  open(unit=95,FILE=trim(uh_state_out_file),FORM='formatted',status='replace')
  print*, 'Writing UH state file: ', trim(uh_state_out_file)
  print*, ' '

  ! each day, writes out uh_length-1 values preceding the current day & also the current day
  ! reasoning is that the other states are for the END of the period, so one would be initializing 
  ! a simulation or forecast starting the following period (or day)

  do i = 1,sim_length
    write(95,44) year(i),month(i),day(i),hour(i),out_tci(i:i+uh_length-1)
  enddo

  close(unit=95)
  deallocate(out_tci)

  return
end subroutine write_uh_state

! cccccccccccccccccccccccccccccccccccccccccc

subroutine read_uh_state(prior_tci,uh_length)
  ! A.Wood, 2016 -- just read prior uh_length + current value for tci
  !   Could read whole output state timeseries if date given to select current init,
  !   but for now let's keep that as a pre-process

  use nrtype
  use def_namelists, only: uh_state_in_file
  implicit none

  !input variable
  integer(I4B), intent(in)	:: uh_length

  !output variables
  real(sp), dimension(:), intent(out) 	:: prior_tci

  !local variables
  integer(I4B)	:: i

  print*, 'Reading UH state file: ', trim(uh_state_in_file)
  print*, ' '
  open(unit=95,FILE=trim(uh_state_in_file),FORM='formatted',status='old')

  do i = 1,uh_length
    read(95,*) prior_tci(i)
  enddo

  close(unit=95)

  return
end subroutine read_uh_state

! cccccccccccccccccccccccccccccccccccccccccc


!ccccccccccccccccccccccccccccccc

subroutine read_snow17_state(cs,tprev)
  use nrtype
  use def_namelists, only: snow17_state_in_file
  implicit none

  !input variables
  real(sp), intent(out) 			:: tprev		!carry over variable
  real(sp), dimension(:), intent(out)	:: cs				!carry over array

  !local variables
  integer(I4B)	:: i

  print*, 'Reading snow state file: ', trim(snow17_state_in_file)
  open(unit=95,FILE=trim(snow17_state_in_file),FORM='formatted',status='old')

  do i = 1,19
    read(95,*) cs(i)
  enddo
  read(95,*) tprev
  close(unit=95)

  return
end subroutine read_snow17_state

! ccccccccccccccccccccccccccccccc

subroutine read_sac_state(uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc)
  use nrtype
  use def_namelists, only: sac_state_in_file
  implicit none

  !input variables
  real(sp), intent(out)	:: uztwc					!state variable
  real(sp), intent(out)	:: uzfwc					!state array
  real(sp), intent(out)	:: lztwc					!state array
  real(sp), intent(out)	:: lzfsc					!state array
  real(sp), intent(out)	:: lzfpc					!state array
  real(sp), intent(out)	:: adimc					!state array

  print*, 'Reading sac state file: ', trim(sac_state_in_file)
  open(unit=95,FILE=trim(sac_state_in_file),FORM='formatted',status='old')

  read(95,*) uztwc
  read(95,*) uzfwc
  read(95,*) lztwc
  read(95,*) lzfsc
  read(95,*) lzfpc
  read(95,*) adimc

  close(unit=95)

  return
end subroutine read_sac_state

!ccccccccccccccccccccccccccccccc

subroutine get_sim_length(sim_length)

  ! TERRIBLE STRATEGY -- reads forcings twice over
  !   use date subroutine

  use nrtype
  use def_namelists, only: forcing_name, start_year,start_day,start_month, &
                        end_year,end_month,end_day

  !output variables
  integer(I4B),intent(out)	:: sim_length

  !local variables
  integer(I4B)			:: ios=0
  integer(I4B)			:: dum_int
  integer(I4B)			:: year,month,day
  integer(I4B)			:: read_flag

  character(len=64), parameter		:: read_format = "(I4.4, 3(1x,I2.2),1x,7(F10.2))"
  character(len = 1024)			:: dum_str
  real(dp)		:: dum_real

  ! === code below ====
  read_flag = 0
  sim_length = 0

  open (UNIT=50,file=trim(forcing_name),form='formatted',status='old')

  read (UNIT=50,FMT='(F7.2)') dum_real
  read (UNIT=50,FMT='(F7.2)') dum_real
  read (UNIT=50,FMT='(F11.0)') dum_real
  read (UNIT=50,FMT='(80A)') dum_str

  do while(ios .ge. 0)
    read (UNIT=50,FMT=read_format,IOSTAT=ios) year,month,day,dum_int,&
				dum_real,dum_real,dum_real,dum_real,dum_real,&
				dum_real,dum_real

    if(year .eq. start_year .and. month .eq. start_month .and. day .eq. start_day) then
      read_flag = 1
    end if

    if(read_flag .eq. 1) then
      sim_length = sim_length + 1
    end if

    if(year .eq. end_year .and. month .eq. end_month .and. day .eq. end_day) then
      read_flag = 0
    end if
  end do

  close(unit=50)
end subroutine get_sim_length

! ccccccccccccccccccccccccccccccc

!subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,vpd,dayl,swdown,precip)
! AWW modified to read PET instead of dayl, vpd and swdown
! AWW modified to return basin area in sq km
subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,precip,pet)
  use nrtype
  use def_namelists, only: forcing_name, start_year,start_day,start_month, &
                        end_year,end_month,end_day,lat,area_basin,elev

  implicit none

  ! output variables
  integer(I4B),dimension(:),intent(out)	:: year
  integer(I4B),dimension(:),intent(out)	:: month
  integer(I4B),dimension(:),intent(out)	:: day
  integer(I4B),dimension(:),intent(out)	:: hour
  real(dp),dimension(:),intent(out)	:: tmin
  real(dp),dimension(:),intent(out)	:: tmax
  !real(dp),dimension(:),intent(out)	:: vpd
  !real(dp),dimension(:),intent(out)	:: dayl
  !real(dp),dimension(:),intent(out)	:: swdown
  real(dp),dimension(:),intent(out)	:: pet
  real(dp),dimension(:),intent(out)	:: precip

  ! local variables
  integer(I4B)				:: i,ios=0
  integer(I4B)				:: yr,mnth,dy,hr
  integer(I4B)				:: read_flag
  !character(len=64), parameter		:: read_format = "(I4.4, 3(1x,I2.2),1x,7(F10.2))"
  character(len=64), parameter		:: read_format = "(I4.4, 3(1x,I2.2),1x,4(F10.2))"
  character(len = 1024)			:: dum_str
  !real(DP)				:: swe
  !real(DP)				:: dum_real
  !real(DP)				:: dl,pcp,sw,tma,tmn,vp
  real(DP)				:: pcp,tma,tmn,pm_pet

  ! -- code below --

  i = 1
  read_flag = 0

  ! read met file
  open (UNIT=50,file=trim(forcing_name),form='formatted',status='old')

  ! read header info
  read (UNIT=50,FMT='(F7.2)') lat
  read (UNIT=50,FMT='(F7.2)') elev
  read (UNIT=50,FMT='(F11.0)') area_basin
  read (UNIT=50,FMT='(80A)') dum_str   ! column labels

  ! read the data, keeping only forcings in simulation period

  do while(ios .ge. 0 .and. read_flag < 2)
    ! forcing could have any format, nice!
    read (UNIT=50,FMT=*,IOSTAT=ios) yr,mnth,dy,hr,pcp,tma,tmn,pm_pet
	!		    dl,pcp,sw,swe,tma,&
	!		    tmn,vp

    if(yr .eq. start_year .and. mnth .eq. start_month .and. dy .eq. start_day) then
      read_flag = 1
    end if

    ! read and store data for simulation period
    if(read_flag .eq. 1) then
      year(i)	= yr
      month(i)	= mnth
      day(i)	= dy
      hour(i)	= hr
      !dayl(i)	= dl
      precip(i)	= pcp
      !swdown(i) = sw
      tmax(i)	= tma
      tmin(i)	= tmn
      !vpd(i)	= vp
      pet(i)	= pm_pet
      i = i + 1
    end if

    if(yr .eq. end_year .and. mnth .eq. end_month .and. dy .eq. end_day) then
      read_flag = 2
    end if

  end do

  close(unit=50)
  return
end subroutine read_areal_forcing

!cccccccccccccccccccccccccccccccccccccccccc
!change & simplify
subroutine read_streamflow(streamflow)
  use nrtype
  use constants, only: sec_day,cfs_cms
  use def_namelists, only: stream_name,area_basin,start_year,start_day,start_month, &
			end_year,end_month,end_day

  implicit none

  !output variables
  real(dp),dimension(:),intent(out)	:: streamflow

  !local variables
  integer(I4B)  :: i
  integer(I4B)  :: yr,mn,dy,gauge,ios=0
  real(dp)      :: flow
  integer(I4B)	:: read_flag

  !this subroutine assumes streamflow is daily data of the following format:
  character(len=64), parameter :: read_format = "(I8.8,1x,I4.4, 2(1x,I2.2),1x,1(F8.2))"

  ! --- code below ---
  read_flag = 0
  i = 1

  ! open streamflow file
  open (UNIT=50,file=stream_name,form='formatted',status='old')

  do while(ios .ge. 0 .and. read_flag < 2)
    read (UNIT=50,FMT=read_format,IOSTAT=ios) gauge,yr,mn,dy,flow

    if(yr .eq. start_year .and. mn .eq. start_month .and. dy .eq. start_day) then
      read_flag = 1
    end if

    if(read_flag == 1) then
      streamflow(i) = flow

      !convert flow to mm/day
      !convert streamflow (cfs) to cms
      streamflow(i) = streamflow(i)*cfs_cms  !now in cubic meters per second

      !need to convert to mm/day
      streamflow(i) = streamflow(i)*sec_day !now in cubic meters per day m^3 day^-1
                                           !m^3/day

      !1 cubic meter per day is 1000 mm per square m -> mm*m^2/day
      streamflow(i) = streamflow(i)*1000./area_basin  !now in mm/day
      
      i = i + 1
    end if

    if(yr .eq. end_year .and. mn .eq. end_month .and. dy .eq. end_day) then
      read_flag = 2
    end if

  end do

  close(unit=50)
  return

end subroutine read_streamflow

! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      subroutines for reading param files: sac, snow17, unit hydrograph & pet
! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

subroutine read_sac_params(param_name)
  use nrtype
  use def_namelists, only: uztwm,uzfwm,uzk,pctim,adimp,zperc,rexp, &
			lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree,riva,side,rserv
  implicit none
 
  !input variables
  character(len=1024),intent(in)	:: param_name
 
  !local variables
  character(len=50)		:: param
  real(sp)			:: value
  integer(I4B)			:: ios=0
 
  open(unit=50,file=trim(param_name),status='old')

  ios = 0 ! AWW
  do while(ios .eq. 0)
    read(unit=50,FMT=*,IOSTAT=ios) param,value

    if(param == 'uztwm') then
      uztwm = value
    else if(param == 'uzfwm') then
      uzfwm = value
    else if(param == 'uzk') then
      uzk = value
    else if(param == 'pctim') then
      pctim = value
    else if(param == 'adimp') then
      adimp = value
    else if(param == 'zperc') then
      zperc = value
    else if(param == 'rexp') then
      rexp = value
    else if(param == 'lztwm') then
      lztwm = value
    else if(param == 'lzfsm') then
      lzfsm = value
    else if(param == 'lzfpm') then
      lzfpm = value
    else if(param == 'lzsk') then
      lzsk = value
    else if(param == 'lzpk') then
      lzpk = value
    else if(param == 'pfree') then
      pfree = value
    else if(param == 'riva') then
      riva = value
    else if(param == 'side') then
      side = value
    else if(param == 'rserv') then
      rserv = value
    end if
  end do
  close(unit=50)

  return
end subroutine read_sac_params

! ccccccccc
subroutine read_snow17_params(param_name)
  use nrtype
  use def_namelists, only: scf,mfmax,mfmin,uadj,si,pxtemp,nmf,&
                        tipm,mbase,plwhc,daygm,adc
  implicit none
 
  !input variables
  character(len=1024),intent(in)	:: param_name

  !local variables
  character(len=50)		:: param
  real(sp)			:: value
  integer(I4B)			:: ios=0  ! Assignment doesn't seem to work

  open(unit=50,file=trim(param_name),status='old')

  ios = 0  ! AWW added 20160823
  do while(ios .eq. 0)
    read(unit=50,FMT=*,IOSTAT=ios) param,value
    if(param == 'mfmax') then
      mfmax = value
    else if(param == 'mfmin') then
      mfmin = value
    else if(param == 'scf') then
      scf = value
    else if(param == 'uadj') then
      uadj = value
    else if(param == 'si') then
      si = value
    else if(param == 'pxtemp') then
      pxtemp = value
    else if(param == 'nmf') then
      nmf = value
    else if(param == 'tipm') then
      tipm = value
    else if(param == 'mbase') then
      mbase = value
    else if(param == 'plwhc') then
      plwhc = value
    else if(param == 'daygm') then
      daygm = value
    else if(param == 'adc1') then
      adc(1) = value
    else if(param == 'adc2') then
      adc(2) = value
    else if(param == 'adc3') then
      adc(3) = value
    else if(param == 'adc4') then
      adc(4) = value
    else if(param == 'adc5') then
      adc(5) = value
    else if(param == 'adc6') then
      adc(6) = value
    else if(param == 'adc7') then
      adc(7) = value
    else if(param == 'adc8') then
      adc(8) = value
    else if(param == 'adc9') then
      adc(9) = value
    else if(param == 'adc10') then
      adc(10) = value
    else if(param == 'adc11') then
      adc(11) = value
    end if
  end do

  close(unit=50)

  return
end subroutine read_snow17_params

! cccccccccccc
subroutine read_uhp_params(param_name)
  use nrtype
  use def_namelists, only: unit_shape,unit_scale,pet_coef
  implicit none
 
  !input variables
  character(len=1024),intent(in)	:: param_name

  !local variables
  character(len=50)		:: param
  real(sp)			:: value
  integer(I4B)			:: ios=0

  open(unit=50,file=trim(param_name),status='old')

  ios = 0  ! AWW added 20160823
  do while(ios .eq. 0)
    read(unit=50,FMT=*,IOSTAT=ios) param,value

    if(param == 'unit_shape') then
      unit_shape = value
    else if(param == 'unit_scale') then
      unit_scale = value
    else if(param == 'pet_coef') then
      pet_coef = value
    end if
  end do
  close(unit=50)

  return
end subroutine read_uhp_params
