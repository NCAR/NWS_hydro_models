
!ccccccccccccccccccccccccccccccccc

subroutine calc_pet_pt(jday,tmax,tmin,tair,vpd,swdown,dayl,pet)
  use constants
  use nrtype
  use interfaces, only: sfc_pressure
  use def_namelists, only: elev,lat,pet_coef

  implicit none

  !input variable
  integer(I4B), dimension(:), intent(in) 	:: jday
  real(dp), dimension(:), intent(in) 		:: tair
  real(dp), dimension(:), intent(in) 		:: tmax
  real(dp), dimension(:), intent(in) 		:: tmin
  real(dp), dimension(:), intent(in) 		:: vpd
  real(dp), dimension(:), intent(in) 		:: swdown
  real(dp), dimension(:), intent(in) 		:: dayl

  !output variable
  real(dp),dimension(:),intent(out)		:: pet

!local variables
  integer(I4B)          :: i
  integer(I4B)		:: end_pt
  
  real(DP)               :: albedo	!albedo for pet calculation
  real(DP)             	 :: apt		!p-t coefficient for aird regions...

  real(DP)               :: l		!latent heat of vaporization (MJ kg-1)
  real(DP)               :: g		!psychrometric constant
  real(DP)               :: s		!slope of the saturation vapour pressure-temperature relationship
  real(DP)               :: tavg	!daily average temperature ( deg C )
  real(DP)               :: r_net	!daily net radiation (MJ m-2 day-1)
  real(DP)               :: r_nl	!daily net longwave (MJ m-2 day-1)
  real(DP)               :: r_ns	!daily net shortwave (MJ m-2 day-1)
  real(DP)               :: pressure	!surface pressure (kPa)
  real(DP)               :: r_s		!daily estimated solar from forcing dataset (MJ m-2 day-1)
  real(DP)               :: r_a		!daily extraterrestrial radiation (MJ m-2 day-1)
  real(DP)               :: r_so	!daily clear sky solar shortwave (MJ m-2 day-1)
  real(DP)               :: d_r		!inverse relative earth-sun distance
  real(DP)               :: dec		!solar declination in radians
  real(DP)               :: lat_rad	!latitude in radians
  real(DP)               :: sha		!sunset hour angle
  real(DP)               :: e_a		!vapor pressure (kPa)
  real(DP)               :: e_s		!saturation vapor pressure (kPa)


!!set pet coefficient from namelist parameter
  apt = real(pet_coef,kind(dp))

!!set albedo to a constant
  albedo = 0.20_dp

!!calculate pressure from elevation using standard atmosphere (taken from Snow-17)
  call sfc_pressure(elev,pressure) !pressure in hPa
  pressure = pressure/10.0_dp !in kPa



!how many days to do?
!  end_pt = size(pet)
  end_pt = size(jday)

!now lets get going on p-t pet
  do i = 1,end_pt
!    tavg = (tmax(i)+tmin(i))/2
    tavg = tair(i)
    l = l_v - tadj*tavg   !(MJ kg-1)
!    l = 2.501 - 0.002361*tavg
    s = slope_svpc_a*exp(slope_svpc_b*tavg)
!    s = 0.04145*exp(0.06088*tavg)
    g = (c_p*pressure)/(e*l)

!    e_s = 0.6108*exp((17.27*tavg)/(tavg+237.3))  !in kPa
    e_s = e_sa*exp((e_sb*tavg)/(tavg+e_sc)) !in kPa
!    e_a = -vpd(i)/1000. + e_s  !in kPa
    e_a = vpd(i)/1000.0_dp

    !radiation terms
!    d_r = 1 + 0.033*cos(((2*pi)/365.) * jday(i))
    d_r = 1 + sun_e_inv_d*cos(((2*pi_d)/365.0_dp) * jday(i))
!    dec = 0.409*sin( (((2*pi)/365.) * jday(i)) - 1.39)
    dec = sol_dec_a*sin( (((2*pi_d)/365.0_dp) * jday(i)) - sol_dec_b)
    lat_rad = (pi_d/180.0_dp)*lat
    sha = acos(-tan(lat_rad)*tan(dec))

    r_a = ((24.*60.)/pi_d)*gsc*d_r*((sha*sin(lat_rad)*sin(dec)) + (cos(lat_rad)*cos(dec)*sin(sha))) !in MJ m-2 day-1
!    r_so = (0.75 + 2e-5*elev)*r_a !in MJ m-2 day-1
    r_so = (clear_sky_a + clear_sky_b*elev)*r_a !in MJ m-2 day-1
!    r_s  = (swdown(i)*dayl(i)/86400.)*0.0864 !in MJ m-2 day-1
    r_s = (swdown(i)*dayl(i)/86400.)*w_to_mj !in MJ m-2 day-1
    r_ns = (1.0_dp-albedo)*r_s  !assume a constant albedo of ~0.20    !in MJ m-2 day-1

!    r_nl = sbc*((((tmax(i)+273.16)**4)+((tmin(i)+273.16)**4))/2)*(0.34-0.14*sqrt(e_a))*(1.35*(r_s/r_so)-0.35) !in MJ m-2 day-1
    r_nl = sbc*((((tmax(i)+273.16)**4)+((tmin(i)+273.16)**4))/2)*(net_lw_a-net_lw_b*sqrt(e_a))*(net_lw_c*(r_s/r_so)-net_lw_d) !in MJ m-2 day-1

    r_net = r_ns - r_nl


    pet(i) = (1./l)*((s*r_net)/(s+g))*apt

  enddo

  return
end subroutine
