get_soilproperties <- function(sand = 0.5, clay = 0.5, model="ED", orc_map="zobler"){

  wdns = 1.000e3
  grav = 9.80665
  day_sec = 86400

  if(model=="ED"){

    theta_sat = (50.5 - 14.2*sand - 3.7*clay)/100 # m³/m³
    psi_sat = -0.01*10^(2.17-1.58*sand-0.63*clay) # m
    b = 3.1-0.3*sand + 15.7*clay #unitless
    k_sat = 7.055556e-6*10^{-0.6+1.26*sand-0.64*clay} # m/s

    fieldcp_K = 0.1 # kg/m²/day
    fieldcp_K_unit = fieldcp_K/wdns/day_sec # m/s
    soilwp_MPa  = -1.5

    # WC at field capacity
    theta_fc = theta_sat*(fieldcp_K_unit/k_sat)**(1/(2*b+3)) # m³/m³

    # WC at wilting point
    theta_wp = theta_sat*(psi_sat/(soilwp_MPa * wdns / grav))^(1/b) # m³/m³

    return(list(theta_sat = theta_sat,
                theta_wp = theta_wp,
                theta_fc = theta_fc,
                b = b,
                k_sat = k_sat,
                psi_sat = -0.01))

  }

  if(model=="LPJ-GUESS"){

    # Derivation of soil properties for LPJ-GUESS.
    # These calculations can be found in the model code: /source/modules/soilinput.cpp::get_mineral()

    silt = 1-sand-clay

    # Some comment in the code:
    # Equation 1 from Cosby 1984
    # Psi = Psi_s * (Theta/Theta_s)^b
    # Psi is the pressure head in cm
    # *_s is the values at saturation
    # Theta is the volumetric moisture content in percent
    # Re-arranged to get the Theta
    # Theta = Theta_s * (Psi/Psi_s)^(1/b)

    b = 3.10 + 15.7*clay - 0.3*sand # from Cosby 1984 (Table 4) (empirical parameter in perc equation, mm/day)
    logPsi_s = 1.54 - 0.95*sand + 0.63*silt
    theta_sat = 0.01 * (50.5 - 14.2 * sand - 3.7 * clay) # saturation capacity, in Cosby expressed as %
    psi_sat = 10^(-logPsi_s)
    psi_wilt = 10^-4.2
    psi_whc = 10^-2
    psi_fc = 10^-2.47
    theta_whc  = theta_sat * (psi_whc  / psi_sat)^(1/b)
    theta_fc  = theta_sat * (psi_fc  / psi_sat)^(1/b)
    theta_wp = theta_sat * (psi_wilt / psi_sat)^(1/b) # wilting point as fraction of depth (Prentice 1992)

    # Then later in the code, for actual use in the rest of the model,
    # b is re-calculated as below, with the following explanation:
    # "A linear dependence between the percolation coefficient from Haxeltine 1996a
    # and the texture dependent parameter b from Cosby 1984 was established
    # K = 5.87 - 0.29*b"

    # b_orig = b # saving it for "return"
    k_sat = 5.87 - 0.29*b # mm/d

    volumetric_whc_field_capacity = theta_whc - theta_wp # volWHC at field capacity minus volWHC at wilting point (Hmax), as fraction of soil depth

    # Thermal diffusivities follow van Duin (1963), Jury et al (1991) Fig 5.11
    thermal_wilting_point = 0.2 # thermal diffusivity (mm2/s) at wilting point (0% WHC)
    thermal_15_whc = 0.15 * b + 0.05  # thermal diffusivity (mm2/s) at 15% WHC
    thermal_field_capacity = 0.4 # thermal diffusivity at field capacity (100% WHC), these di

    # Other values are also set, they are read directly from the soil input map.
    # Only organic content fraction (orgC) is really required.
    #   pH
    #   orgC
    #   soilC
    #   C:N
    #   bulkdensity

    return(list(theta_sat = theta_sat,
                theta_wp = theta_wp,
                theta_fc = theta_whc, # I suppose this is the same or theta_fc
                b = b,
                k_sat = k_sat/86400/1000, # not calculated I guess?
                psi_sat = psi_sat)) # maybe we'll need to add a minus sign?


  }

  if(model=="ORCHIDEE"){ # ORCHIDEE_2.2 used in CMIP6
    # Derivation of soil properties for ORCHIDEE.

    # ORCHIDEE trunk v2.2 uses a look-up tables from Carsel and Parrish (1988) to get soil parameters from soil texture
    # Two maps can be used, the one of Zobler (1986) - 5 classes simplified into 3 classes COARSE-MEDIUM-FINE (this is the one used for CMIP6),
    # or the one from Reynolds (2000) - 11 classes
    # For a complete description of hydrological processes, please refers to http://forge.ipsl.jussieu.fr/orchidee/attachment/wiki/Documentation/eqs_hydrol_25April2018_Ducharne.pdf

    # we work here with the zobler map, the most used, but both are implemented
    # 1. Definition of soil classes depending on %clay,%sand,%silt
    clay=clay*100; sand=sand*100
    silt = 100-sand-clay
    usda_class<-soiltexture::TT.points.in.classes( tri.data = data.frame(CLAY=clay,
                                                                         SILT=silt,
                                                                         SAND=sand)
                                                   ,class.sys   = "USDA.TT",
                                                   PiC.type = "t")
    # USDA classification:
    # "Cl"     "SiCl"   "SaCl"   "ClLo"   "SiClLo" "SaClLo" "Lo"     "SiLo"   "SaLo"   "Si"     "LoSa"   "Sa"
    # Zobler classification
    # "LoSa"(1), "SaLo"(2), "Lo" (3), "SaClLo" (4), and "ClLo" (5),
    # which are reduced to
    # Coarse == (1), Medium == (2,3,4), and Fine (5)


    if (orc_map=="zobler"){
      # we convert USDA to Coarse, Medium and Fine
      if (!all(usda_class[1,1:5]==0)){ # case usda ==fine
        soil_class<-3
      } else if (!all(usda_class[1,6:10]==0)){
        soil_class<-2
      } else {
        soil_class<-1
      }

      # constant by class from src_parameters/constantes_soil_var.f90
      #  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: ks_fao = &              !! Hydraulic conductivity at saturation
      # & (/ 1060.8_r_std, 249.6_r_std, 62.4_r_std /)                         !!  @tex $(mm d^{-1})$ @endtex
      ks_fao<-c(1060.8,249.6,62.4) # * 86400 / 1000 # mm d-1 --> m s-1

      # REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: mcs_fao = &             !! Saturated volumetric water content
      # & (/ 0.41_r_std, 0.43_r_std, 0.41_r_std /)                            !!  @tex $(m^{3} m^{-3})$ @endtex
      theta_s_fao<-c(0.41,0.43,0.41)

      # We use the VG relationships to derive mcw and mcf depending on soil texture
      # assuming that the matric potential for wilting point and field capacity is
      # -150m (permanent WP) and -3.3m respectively
      # (-1m for FC for the three sandy soils following Richards, L.A. and Weaver, L.R. (1944)
      # !! Note that mcw GE mcr
      # REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: mcf_fao = &             !! Volumetric water content at field capacity
      # & (/ 0.1218_r_std, 0.1654_r_std, 0.2697_r_std /)                      !!  @tex $(m^{3} m^{-3})$ @endtex
      theta_fc_fao<-c(0.1218,0.1654,0.2697)
      # REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: mcw_fao = &             !! Volumetric water content at wilting point
      # & (/ 0.0657_r_std,  0.0884_r_std, 0.1496_r_std/)                      !!  @tex $(m^{3} m^{-3})$ @endtex
      theta_wp_fao<-c(0.0657,0.0884,0.1496)
      # REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: mcr_fao = &             !! Residual volumetric water content
      # & (/ 0.065_r_std, 0.078_r_std, 0.095_r_std /)                         !!  @tex $(m^{3} m^{-3})$ @endtex
      theta_res_fao<-c(0.065,0.078,0.095)
      # REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: nvan_fao = &            !! Van Genuchten coefficient n (unitless)
      # & (/ 1.89_r_std, 1.56_r_std, 1.31_r_std /)                             !  RK: 1/n=1-m
      n_fao<-c(1.89,1.56,1.31)
      # REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: avan_fao = &            !! Van Genuchten coefficient a
      # & (/ 0.0075_r_std, 0.0036_r_std, 0.0019_r_std /)                     !!  @tex $(mm^{-1})$ @endtex
      alpha_fao<-c(0.0075,0.0036,0.0019)

      # k_sat is a function of soil depth to account for compression. Add it here?
      k_sat<-ks_fao[soil_class]
      theta_sat<-theta_s_fao[soil_class]
      theta_fc<-theta_fc_fao[soil_class]
      theta_wp<-theta_wp_fao[soil_class]
      theta_res<-theta_res_fao[soil_class]
      n<-n_fao[soil_class]
      alpha<-alpha_fao[soil_class]

    } else if (orc_map=="usda"){
      # from soiltexture
      # "Cl"     "SiCl"   "SaCl"   "ClLo"   "SiClLo" "SaClLo" "Lo"     "SiLo"   "SaLo"   "Si"     "LoSa"   "Sa"
      # in Orchidee
      # "Sa" "LoSa" "SaLo" "SiLo" "Si" "Lo" "SaClLo" "SiClLo" "ClLo" "SaCl" "SiCl" "Cl"
      mapping <-  c("Sa","LoSa","SaLo","SiLo","Si","Lo","SaClLo","SiClLo","ClLo","SaCl","SiCl","Cl")
      ord<-c(12,11,9,8,10,7:1)
      usda_class <- as.factor(usda_class)
      soil_class <- match(usda_class,mapping)
      # soil_class<-usda_class[,ord]

      # REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: ks_usda = &              !! Hydraulic conductivity at saturation
      # & (/ 7128.0_r_std, 3501.6_r_std, 1060.8_r_std, 108.0_r_std, &           !!  @tex $(mm d^{-1})$ @endtex
      #    &    60.0_r_std, 249.6_r_std, 314.4_r_std, 16.8_r_std, &
      #      &    62.4_r_std, 28.8_r_std, 4.8_r_std, 48.0_r_std /)
      ks_usda<-c(7128.0,3501.6,1060.8,108.0,60.0,249.6,314.4,16.8,62.4,28.8,4.8,48.0)/wdns/day_sec

      # REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: nvan_usda = &            !! Van Genuchten coefficient n (unitless)
      # & (/ 2.68_r_std, 2.28_r_std, 1.89_r_std, 1.41_r_std, &                   !  RK: 1/n=1-m
      #    &    1.37_r_std, 1.56_r_std, 1.48_r_std, 1.23_r_std, &
      #      &    1.31_r_std, 1.23_r_std, 1.09_r_std, 1.09_r_std /)
      n_usda<-c(2.68,2.28,1.89,1.41,1.37,1.56,1.48,1.23,1.31,1.23,1.09,1.09)

      # REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: avan_usda = &            !! Van Genuchten coefficient a
      # & (/ 0.0145_r_std, 0.0124_r_std, 0.0075_r_std, 0.0020_r_std, &          !!  @tex $(mm^{-1})$ @endtex
      #    &    0.0016_r_std, 0.0036_r_std, 0.0059_r_std, 0.0010_r_std, &
      #      &    0.0019_r_std, 0.0027_r_std, 0.0005_r_std, 0.0008_r_std /)
      alpha_usda<-c(0.0145,0.0124,0.0075,0.0020,0.0016,0.0036,0.0059,0.0010,0.0019,0.0027,0.0005,0.0008)

      # REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: mcr_usda = &             !! Residual volumetric water content
      # & (/ 0.045_r_std, 0.057_r_std, 0.065_r_std, 0.067_r_std, &              !!  @tex $(m^{3} m^{-3})$ @endtex
      #    &    0.034_r_std, 0.078_r_std, 0.100_r_std, 0.089_r_std, &
      #      &    0.095_r_std, 0.100_r_std, 0.070_r_std, 0.068_r_std /)
      theta_res_usda<-c(0.045,0.057,0.065,0.067,0.034,0.078,0.1,0.089,0.095,0.1,0.070,0.068)

      # REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: mcs_usda = &             !! Saturated volumetric water content
      # & (/ 0.43_r_std, 0.41_r_std, 0.41_r_std, 0.45_r_std, &                  !!  @tex $(m^{3} m^{-3})$ @endtex
      #    &    0.46_r_std, 0.43_r_std, 0.39_r_std, 0.43_r_std, &
      #      &    0.41_r_std, 0.38_r_std, 0.36_r_std, 0.38_r_std /)
      theta_s_usda<-c(0.43,0.41,0.41,0.45,0.46,0.43,0.39,0.43,0.41,0.38,0.36,0.38)

      # REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: mcf_usda = &             !! Volumetric water content at field capacity
      # & (/ 0.0493_r_std, 0.0710_r_std, 0.1218_r_std, 0.2402_r_std, &          !!  @tex $(m^{3} m^{-3})$ @endtex
      #    0.2582_r_std, 0.1654_r_std, 0.1695_r_std, 0.3383_r_std, &
      #      0.2697_r_std, 0.2672_r_std, 0.3370_r_std, 0.3469_r_std /)
      theta_fc_usda<-c(0.0493,0.0710,0.1218,0.2402,0.2582,0.1654,0.1695,0.3383,0.2697,0.2672,0.3370,0.3469)

      # REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: mcw_usda = &             !! Volumetric water content at wilting point
      # & (/ 0.0450_r_std, 0.0570_r_std, 0.0657_r_std, 0.1039_r_std, &          !!  @tex $(m^{3} m^{-3})$ @endtex
      #    0.0901_r_std, 0.0884_r_std, 0.1112_r_std, 0.1967_r_std, &
      #      0.1496_r_std, 0.1704_r_std, 0.2665_r_std, 0.2707_r_std /)
      theta_wp_usda<-c(0.0450,0.0570,0.0657,0.1039,0.0901,0.0884,0.1112,0.1967,0.1496,0.1704,0.2665,0.2707)

      # k_sat is a function of soil depth to account for compression. Add it here?
      k_sat<-ks_usda[soil_class]
      theta_sat<-theta_s_usda[soil_class]
      theta_fc<-theta_fc_usda[soil_class]
      theta_wp<-theta_wp_usda[soil_class]
      theta_res<-theta_res_usda[soil_class]
      n<-n_usda[soil_class]
      alpha<-alpha_usda[soil_class]
    } else {
      print("ORCHIDEE need soil classification: zobler or usda")
    }

    # in the rest of the model the unsaturated values of hydraulic conductivity and diffusivity are
    # given by the model of Mualem (1976) and Van Genuchten (1980).
    # psi_sat is replaced by m and alpha here.

    return(list(theta_sat = theta_sat,
                theta_wp = theta_wp,
                theta_fc = theta_fc,
                theta_res = theta_res,
                b = NA, # not used in orchidee
                k_sat = k_sat,
                psi_sat = NA, # not used in orchidee
                n=n, # Van Genuchten parameter needed for K and D Van Genuchten(1980)
                alpha=alpha # inverse of the air entry suction needed for K and D Van Genuchten (1980)
    ))


  }

}
