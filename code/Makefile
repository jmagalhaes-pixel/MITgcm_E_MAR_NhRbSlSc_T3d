# Multithreaded + multi-processing makefile for:
#    Linux kraken 2.6.32-358.el6.x86_64 #1 SMP Tue Jan 29 11:47:41 EST 2013 x86_64 x86_64 x86_64 GNU/Linux
# This makefile was generated automatically on
#    Fri Nov 28 17:24:16 CST 2014
# by the command:
#    /model_output2/intwave/MITgcm_c62o/tools/genmake2 -rootdir=/model_output2/intwave/MITgcm_c62o -mods=../code -mpi -of=/model_output2/intwave/MITgcm_c62o/tools/build_options/linux_amd64_ifort_kraken -disable=mnc
# executed by:
#    ecom@kraken:/model_output2/intwave/SIMS/virtual07/code
#
# OPTFILE="/model_output2/intwave/MITgcm_c62o/tools/build_options/linux_amd64_ifort_kraken"
#
# BUILDDIR     : Directory where object files are written
# SOURCEDIRS   : Directories containing the source (.F) files
# INCLUDEDIRS  : Directories containing the header-source (.h) files
# EXEDIR       : Directory where executable that is generated is written
# EXECUTABLE   : Full path of executable binary
#
# CPP          : C-preprocessor command
# INCLUDES     : Directories searched for header files
# DEFINES      : Macro definitions for CPP
# MAKEDEPEND   : Dependency generator
# KPP          : Special preprocessor command (specific to platform)
# KFLAGS       : Flags for KPP
# FC           : Fortran compiler command
# FFLAGS       : Configuration/debugging options for FC
# FOPTIM       : Optimization options for FC
# LINK         : Command for link editor program
# LIBS         : Library flags /or/ additional optimization/debugging flags

ROOTDIR     = /model_output2/intwave/MITgcm_c62o
BUILDDIR    = .
SOURCEDIRS  =  ../code /model_output2/intwave/MITgcm_c62o/pkg/debug /model_output2/intwave/MITgcm_c62o/pkg/generic_advdiff /model_output2/intwave/MITgcm_c62o/pkg/mdsio /model_output2/intwave/MITgcm_c62o/pkg/mom_common /model_output2/intwave/MITgcm_c62o/pkg/mom_fluxform /model_output2/intwave/MITgcm_c62o/pkg/mom_vecinv /model_output2/intwave/MITgcm_c62o/pkg/monitor /model_output2/intwave/MITgcm_c62o/pkg/obcs /model_output2/intwave/MITgcm_c62o/pkg/pp81 /model_output2/intwave/MITgcm_c62o/pkg/rw /model_output2/intwave/MITgcm_c62o/pkg/timeave /model_output2/intwave/MITgcm_c62o/eesupp/src /model_output2/intwave/MITgcm_c62o/model/src
INCLUDEDIRS = /shared/netcdf-4.1.2-intel//include /shared/intel/impi/4.0.2.003/intel64/include ../code /model_output2/intwave/MITgcm_c62o/pkg/debug /model_output2/intwave/MITgcm_c62o/pkg/generic_advdiff /model_output2/intwave/MITgcm_c62o/pkg/mdsio /model_output2/intwave/MITgcm_c62o/pkg/mom_common /model_output2/intwave/MITgcm_c62o/pkg/mom_fluxform /model_output2/intwave/MITgcm_c62o/pkg/mom_vecinv /model_output2/intwave/MITgcm_c62o/pkg/monitor /model_output2/intwave/MITgcm_c62o/pkg/obcs /model_output2/intwave/MITgcm_c62o/pkg/pp81 /model_output2/intwave/MITgcm_c62o/pkg/rw /model_output2/intwave/MITgcm_c62o/pkg/timeave /model_output2/intwave/MITgcm_c62o/eesupp/inc /model_output2/intwave/MITgcm_c62o/model/inc
EXEDIR      = .
EXECUTABLE  = $(EXEDIR)/mitgcmuv
TOOLSDIR    = /model_output2/intwave/MITgcm_c62o/tools

#eh3  new defines for the adjoint work
AUTODIFF    = /model_output2/intwave/MITgcm_c62o/pkg/autodiff
EXE_AD      = mitgcmuv_ad
EXE_FTL     = mitgcmuv_ftl
EXE_SVD     = mitgcmuv_svd

ENABLED_PACKAGES =  -DALLOW_DEBUG -DALLOW_GENERIC_ADVDIFF -DALLOW_MDSIO -DALLOW_MOM_COMMON -DALLOW_MOM_FLUXFORM -DALLOW_MOM_VECINV -DALLOW_MONITOR -DALLOW_OBCS -DALLOW_PP81 -DALLOW_RW -DALLOW_TIMEAVE
DISABLED_PACKAGES =  -UALLOW_ADMTLM -UALLOW_AIM_COMPON_INTERF -UALLOW_AIM_V23 -UALLOW_ATM2D -UALLOW_ATM_COMMON -UALLOW_ATM_OCN_COUPLER -UALLOW_AUTODIFF -UALLOW_BULK_FORCE -UALLOW_CAL -UALLOW_CD_CODE -UALLOW_CFC -UALLOW_CHEAPAML -UALLOW_CHRONOS -UALLOW_COMPON_COMMUNIC -UALLOW_COST -UALLOW_CTRL -UALLOW_DIAGNOSTICS -UALLOW_DIC -UALLOW_DOWN_SLOPE -UALLOW_EBM -UALLOW_ECCO -UALLOW_EMBED_FILES -UALLOW_EXCH2 -UALLOW_EXF -UALLOW_FIZHI -UALLOW_FLT -UALLOW_GCHEM -UALLOW_GGL90 -UALLOW_GMREDI -UALLOW_GRDCHK -UALLOW_GRIDALT -UALLOW_ICEFRONT -UALLOW_KPP -UALLOW_LAND -UALLOW_LAYERS -UALLOW_LONGSTEP -UALLOW_MATRIX -UALLOW_MNC -UALLOW_MY82 -UALLOW_MYPACKAGE -UALLOW_OCN_COMPON_INTERF -UALLOW_OFFLINE -UALLOW_OPPS -UALLOW_PROFILES -UALLOW_PTRACERS -UALLOW_RBCS -UALLOW_REGRID -UALLOW_RUNCLOCK -UALLOW_SALT_PLUME -UALLOW_SBO -UALLOW_SEAICE -UALLOW_SHAP_FILT -UALLOW_SHELFICE -UALLOW_SHOWFLOPS -UALLOW_SMOOTH -UALLOW_SPHERE -UALLOW_THSICE -UALLOW_ZONAL_FILT

# These files are created by Makefile
SPECIAL_FILES = PACKAGES_CONFIG.h AD_CONFIG.h FC_NAMEMANGLE.h BUILD_INFO.h
EMBEDDED_FILES = 
# Unix ln (link)
LN = ln -s
# C preprocessor
CPP = cat $< | $(TOOLSDIR)/set64bitConst.sh |  cpp -traditional -P
# Dependency generator
MAKEDEPEND = $(TOOLSDIR)/cyrus-imapd-makedepend/makedepend
# Special preprocessor (KAP on DECs, FPP on Crays)
KPP = 
# Fortran compiler
FC = mpiifort
# Fortran compiler
F90C = mpiifort
# C compiler
CC = mpicc
# Link editor
LINK = mpiifort -i-dynamic -no-ipo 

# Defines for CPP
DEFINES = -DWORDLENGTH=4 -DALLOW_USE_MPI -DALWAYS_USE_MPI  -DALLOW_USE_MPI -DALWAYS_USE_MPI -DHAVE_SYSTEM -DHAVE_FDATE -DHAVE_ETIME -DHAVE_CLOC -DHAVE_SETRLSTK -DHAVE_SIGREG -DHAVE_STAT 
# Includes for CPP
INCLUDES = -I/shared/netcdf-4.1.2-intel//include -I/shared/intel/impi/4.0.2.003/intel64/include
# Flags for KPP
KFLAGS1 = 
KFLAGS2 = 
# Optim./debug for FC
FFLAGS =  -W0 -WB -convert big_endian -assume byterecl -fPIC 
FOPTIM = -O2 -align 
# Optim./debug for FC
F90FLAGS =  -W0 -WB -convert big_endian -assume byterecl -fPIC
F90OPTIM = -O2 -align 
F90FIXEDFORMAT = -fixed -Tf
# Flags for CC
CFLAGS = -O0 -fPIC
# Files that should not be optimized
NOOPTFILES = 
NOOPTFLAGS = -O0 -g
# Flags and libraries needed for linking
LIBS = -L/shared/netcdf-4.1.2-intel//lib -lnetcdff -lnetcdf
# Name of the Mekfile
MAKEFILE=Makefile

F77_SRC_FILES =  \
 external_forcing.F \
 pp81_calc.F \
 pp81_do_diags.F \
 pp81_output.F \
 chksum_tiled.F \
 debug_call.F \
 debug_cs_corner_uv.F \
 debug_enter.F \
 debug_fld_stats_rl.F \
 debug_fld_stats_rs.F \
 debug_leave.F \
 debug_msg.F \
 debug_stats_rl.F \
 debug_stats_rs.F \
 fill_in_corners_rl.F \
 write_fullarray_rl.F \
 gad_advection.F \
 gad_biharm_x.F \
 gad_biharm_y.F \
 gad_c2_adv_r.F \
 gad_c2_adv_x.F \
 gad_c2_adv_y.F \
 gad_c2_impl_r.F \
 gad_c4_adv_r.F \
 gad_c4_adv_x.F \
 gad_c4_adv_y.F \
 gad_calc_rhs.F \
 gad_check.F \
 gad_del2.F \
 gad_diagnostics_init.F \
 gad_diagnostics_state.F \
 gad_diff_r.F \
 gad_diff_x.F \
 gad_diff_y.F \
 gad_dst2u1_adv_r.F \
 gad_dst2u1_adv_x.F \
 gad_dst2u1_adv_y.F \
 gad_dst2u1_impl_r.F \
 gad_dst3_adv_r.F \
 gad_dst3_adv_x.F \
 gad_dst3_adv_y.F \
 gad_dst3fl_adv_r.F \
 gad_dst3fl_adv_x.F \
 gad_dst3fl_adv_y.F \
 gad_dst3fl_impl_r.F \
 gad_exch_som.F \
 gad_fluxlimit_adv_r.F \
 gad_fluxlimit_adv_x.F \
 gad_fluxlimit_adv_y.F \
 gad_fluxlimit_impl_r.F \
 gad_grad_x.F \
 gad_grad_y.F \
 gad_implicit_r.F \
 gad_init_fixed.F \
 gad_init_varia.F \
 gad_os7mp_adv_r.F \
 gad_os7mp_adv_x.F \
 gad_os7mp_adv_y.F \
 gad_read_pickup.F \
 gad_som_advect.F \
 gad_som_adv_r.F \
 gad_som_adv_x.F \
 gad_som_adv_y.F \
 gad_som_exchanges.F \
 gad_som_fill_cs_corner.F \
 gad_som_lim_r.F \
 gad_som_prep_cs_corner.F \
 gad_u3_adv_r.F \
 gad_u3_adv_x.F \
 gad_u3_adv_y.F \
 gad_u3c4_impl_r.F \
 gad_valid_advscheme.F \
 gad_write_pickup.F \
 salt_fill.F \
 mdsio_buffertorl.F \
 mdsio_buffertors.F \
 mdsio_facef_read.F \
 mdsio_gl.F \
 mdsio_gl_slice.F \
 mdsio_pass_r4torl.F \
 mdsio_pass_r4tors.F \
 mdsio_pass_r8torl.F \
 mdsio_pass_r8tors.F \
 mdsio_pass_wh_r4torl.F \
 mdsio_pass_wh_r4tors.F \
 mdsio_pass_wh_r8torl.F \
 mdsio_pass_wh_r8tors.F \
 mdsio_rd_rec_rl.F \
 mdsio_rd_rec_rs.F \
 mdsio_read_field.F \
 mdsio_read_meta.F \
 mdsio_read_section.F \
 mdsio_readvector.F \
 mdsio_read_whalos.F \
 mdsio_rw_field.F \
 mdsio_rw_slice.F \
 mdsio_seg4torl.F \
 mdsio_seg4tors.F \
 mdsio_seg8torl.F \
 mdsio_seg8tors.F \
 mdsio_segxtorx_2d.F \
 mdsio_write_field.F \
 mdsio_writelocal.F \
 mdsio_write_meta.F \
 mdsio_write_section.F \
 mdsio_writevec_loc.F \
 mdsio_writevector.F \
 mdsio_write_whalos.F \
 mdsio_wr_metafiles.F \
 mdsio_wr_rec_rl.F \
 mdsio_wr_rec_rs.F \
 mom_calc_absvort3.F \
 mom_calc_hdiv.F \
 mom_calc_hfacz.F \
 mom_calc_ke.F \
 mom_calc_relvort3.F \
 mom_calc_strain.F \
 mom_calc_tension.F \
 mom_calc_visc.F \
 mom_diagnostics_init.F \
 mom_hdissip.F \
 mom_init_fixed.F \
 mom_quasihydrostatic.F \
 mom_u_bottomdrag.F \
 mom_u_coriolis_nh.F \
 mom_u_implicit_r.F \
 mom_u_metric_nh.F \
 mom_u_rviscflux.F \
 mom_u_sidedrag.F \
 mom_v_bottomdrag.F \
 mom_v_coriolis_nh.F \
 mom_v_implicit_r.F \
 mom_v_metric_nh.F \
 mom_v_rviscflux.F \
 mom_v_sidedrag.F \
 mom_w_coriolis_nh.F \
 mom_w_metric_nh.F \
 mom_w_sidedrag.F \
 mom_calc_rtrans.F \
 mom_fluxform.F \
 mom_u_adv_uu.F \
 mom_u_adv_vu.F \
 mom_u_adv_wu.F \
 mom_u_coriolis.F \
 mom_u_del2u.F \
 mom_u_metric_cylinder.F \
 mom_u_metric_sphere.F \
 mom_uv_boundary.F \
 mom_u_xviscflux.F \
 mom_u_yviscflux.F \
 mom_v_adv_uv.F \
 mom_v_adv_vv.F \
 mom_v_adv_wv.F \
 mom_v_coriolis.F \
 mom_v_del2v.F \
 mom_v_metric_cylinder.F \
 mom_v_metric_sphere.F \
 mom_v_xviscflux.F \
 mom_v_yviscflux.F \
 mom_vecinv.F \
 mom_vi_coriolis.F \
 mom_vi_del2uv.F \
 mom_vi_hdissip.F \
 mom_vi_u_coriolis_c4.F \
 mom_vi_u_coriolis.F \
 mom_vi_u_grad_ke.F \
 mom_vi_u_vertshear.F \
 mom_vi_v_coriolis_c4.F \
 mom_vi_v_coriolis.F \
 mom_vi_v_grad_ke.F \
 mom_vi_v_vertshear.F \
 mon_advcfl.F \
 mon_advcflw2.F \
 mon_advcflw.F \
 mon_calc_stats_rl.F \
 mon_calc_stats_rs.F \
 mon_init.F \
 monitor_ad.F \
 monitor.F \
 monitor_g.F \
 mon_ke.F \
 mon_out.F \
 mon_printstats_rl.F \
 mon_printstats_rs.F \
 mon_set_iounit.F \
 mon_set_pref.F \
 mon_solution.F \
 mon_stats_latbnd_rl.F \
 mon_stats_rl.F \
 mon_stats_rs.F \
 mon_surfcor.F \
 mon_vort3.F \
 mon_writestats_rl.F \
 mon_writestats_rs.F \
 obcs_apply_eta.F \
 obcs_apply_ptracer.F \
 obcs_apply_r_star.F \
 obcs_apply_seaice.F \
 obcs_apply_sloc.F \
 obcs_apply_surf_dr.F \
 obcs_apply_tloc.F \
 obcs_apply_ts.F \
 obcs_apply_uv.F \
 obcs_apply_uvice.F \
 obcs_apply_w.F \
 obcs_calc.F \
 obcs_calc_stevens.F \
 obcs_check_depths.F \
 obcs_check.F \
 obcs_copy_tracer.F \
 obcs_external_fields_load.F \
 obcs_init_fixed.F \
 obcs_init_variables.F \
 obcs_output.F \
 obcs_prescribe_exchanges.F \
 obcs_prescribe_read.F \
 obcs_readparms.F \
 obcs_read_pickup.F \
 obcs_sponge.F \
 obcs_write_pickup.F \
 orlanski_east.F \
 orlanski_init.F \
 orlanski_north.F \
 orlanski_south.F \
 orlanski_west.F \
 pp81_calc_diff.F \
 pp81_calc_visc.F \
 pp81_check.F \
 pp81_init_varia.F \
 pp81_readparms.F \
 pp81_ri_number.F \
 get_write_global_fld.F \
 read_fld_xy_rl.F \
 read_fld_xy_rs.F \
 read_fld_xyz_rl.F \
 read_fld_xyz_rs.F \
 read_mflds.F \
 read_rec.F \
 set_write_global_fld.F \
 write_fld_3d_rl.F \
 write_fld_3d_rs.F \
 write_fld_s3d_rl.F \
 write_fld_s3d_rs.F \
 write_fld_xy_rl.F \
 write_fld_xy_rs.F \
 write_fld_xyz_rl.F \
 write_fld_xyz_rs.F \
 write_glvec_rl.F \
 write_glvec_rs.F \
 write_local_rl.F \
 write_local_rs.F \
 write_rec.F \
 timeave_cumul_1k1t.F \
 timeave_cumul_1t.F \
 timeave_cumul_1vfc.F \
 timeave_cumul_2v.F \
 timeave_cumul_2vfc.F \
 timeave_cumulate.F \
 timeave_cumul_dif_1t.F \
 timeave_init_fixed.F \
 timeave_norma_2v.F \
 timeave_normalize.F \
 timeave_normaliz.F \
 timeave_reset.F \
 timeave_statvars.F \
 timeave_statv_write.F \
 timeave_surf_flux.F \
 all_proc_die.F \
 bar2.F \
 bar_check.F \
 barrier.F \
 check_threads.F \
 comm_stats.F \
 different_multiple.F \
 diff_phase_multiple.F \
 eeboot.F \
 eeboot_minimal.F \
 eedata_example.F \
 eedie.F \
 eeintro_msg.F \
 eeset_parms.F \
 eewrite_eeenv.F \
 exch1_bg_r4_cube.F \
 exch1_bg_r8_cube.F \
 exch1_bg_rl_cube.F \
 exch1_bg_rs_cube.F \
 exch1_r4_cube.F \
 exch1_r4.F \
 exch1_r8_cube.F \
 exch1_r8.F \
 exch1_rl_ad.F \
 exch1_rl_cube_ad.F \
 exch1_rl_cube.F \
 exch1_rl.F \
 exch1_rs_ad.F \
 exch1_rs_cube_ad.F \
 exch1_rs_cube.F \
 exch1_rs.F \
 exch1_uv_r4_cube.F \
 exch1_uv_r8_cube.F \
 exch1_uv_rl_cube.F \
 exch1_uv_rs_cube.F \
 exch1_z_r4_cube.F \
 exch1_z_r8_cube.F \
 exch1_z_rl_cube.F \
 exch1_z_rs_cube.F \
 exch_3d_r4.F \
 exch_3d_r8.F \
 exch_3d_rl.F \
 exch_3d_rs.F \
 exch_cycle_ebl.F \
 exch_init.F \
 exch_r4_recv_get_x.F \
 exch_r4_recv_get_y.F \
 exch_r4_send_put_x.F \
 exch_r4_send_put_y.F \
 exch_r8_recv_get_x.F \
 exch_r8_recv_get_y.F \
 exch_r8_send_put_x.F \
 exch_r8_send_put_y.F \
 exch_rl_recv_get_x.F \
 exch_rl_recv_get_y.F \
 exch_rl_send_put_x.F \
 exch_rl_send_put_y.F \
 exch_rs_recv_get_x.F \
 exch_rs_recv_get_y.F \
 exch_rs_send_put_x.F \
 exch_rs_send_put_y.F \
 exch_s3d_r4.F \
 exch_s3d_r8.F \
 exch_s3d_rl.F \
 exch_s3d_rs.F \
 exch_sm_3d_r4.F \
 exch_sm_3d_r8.F \
 exch_sm_3d_rl.F \
 exch_sm_3d_rs.F \
 exch_uv_3d_r4.F \
 exch_uv_3d_r8.F \
 exch_uv_3d_rl.F \
 exch_uv_3d_rs.F \
 exch_uv_agrid_3d_r4.F \
 exch_uv_agrid_3d_r8.F \
 exch_uv_agrid_3d_rl.F \
 exch_uv_agrid_3d_rs.F \
 exch_uv_bgrid_3d_r4.F \
 exch_uv_bgrid_3d_r8.F \
 exch_uv_bgrid_3d_rl.F \
 exch_uv_bgrid_3d_rs.F \
 exch_uv_dgrid_3d_r4.F \
 exch_uv_dgrid_3d_r8.F \
 exch_uv_dgrid_3d_rl.F \
 exch_uv_dgrid_3d_rs.F \
 exch_uv_xy_r4.F \
 exch_uv_xy_r8.F \
 exch_uv_xy_rl.F \
 exch_uv_xy_rs.F \
 exch_uv_xyz_r4.F \
 exch_uv_xyz_r8.F \
 exch_uv_xyz_rl.F \
 exch_uv_xyz_rs.F \
 exch_xy_r4.F \
 exch_xy_r8.F \
 exch_xy_rl.F \
 exch_xy_rs.F \
 exch_xyz_r4.F \
 exch_xyz_r8.F \
 exch_xyz_rl.F \
 exch_xyz_rs.F \
 exch_z_3d_r4.F \
 exch_z_3d_r8.F \
 exch_z_3d_rl.F \
 exch_z_3d_rs.F \
 fill_cs_corner_ag_rl.F \
 fill_cs_corner_tr_rl.F \
 fill_cs_corner_uv_rs.F \
 fool_the_compiler.F \
 gather_2d_r4.F \
 gather_2d_r8.F \
 gather_2d_wh_r4.F \
 gather_2d_wh_r8.F \
 gather_vector.F \
 gather_xz.F \
 gather_yz.F \
 global_max.F \
 global_sum.F \
 global_sum_singlecpu.F \
 global_sum_tile.F \
 global_vec_sum.F \
 gsum.F \
 ini_communication_patterns.F \
 ini_procs.F \
 ini_threading_environment.F \
 lef_zero.F \
 main.F \
 master_cpu_io.F \
 master_cpu_thread.F \
 mds_byteswapi4.F \
 mds_byteswapr4.F \
 mds_byteswapr8.F \
 mds_check4file.F \
 mdsfindunit.F \
 mds_reclen.F \
 memsync.F \
 nml_filter.F \
 nml_set_terminator.F \
 open_copy_data_file.F \
 print.F \
 scatter_2d_r4.F \
 scatter_2d_r8.F \
 scatter_2d_wh_r4.F \
 scatter_2d_wh_r8.F \
 scatter_vector.F \
 scatter_xz.F \
 scatter_yz.F \
 stop_if_error.F \
 timers.F \
 utils.F \
 write_utils.F \
 adams_bashforth2.F \
 adams_bashforth3.F \
 calc_3d_diffusivity.F \
 calc_buoyancy.F \
 calc_common_factors.F \
 calc_diffusivity.F \
 calc_div_ghat.F \
 calc_grad_phi_hyd.F \
 calc_grad_phi_surf.F \
 calc_gs.F \
 calc_gt.F \
 calc_gw.F \
 calc_ivdc.F \
 calc_oce_mxlayer.F \
 calc_phi_hyd.F \
 calc_r_star.F \
 calc_surf_dr.F \
 calc_viscosity.F \
 calc_wsurf_tr.F \
 cg2d.F \
 cg2d_nsa.F \
 cg2d_sr.F \
 cg3d.F \
 check_pickup.F \
 config_check.F \
 config_summary.F \
 convective_adjustment.F \
 convective_adjustment_ini.F \
 convectively_mixtracer.F \
 convective_weights.F \
 correction_step.F \
 cycle_tracer.F \
 diags_oceanic_surf_flux.F \
 diags_phi_hyd.F \
 diags_phi_rlow.F \
 diags_rho.F \
 do_atmospheric_phys.F \
 do_fields_blocking_exchanges.F \
 do_gterm_blocking_exchanges.F \
 do_oceanic_phys.F \
 do_stagger_fields_exchanges.F \
 do_statevars_diags.F \
 do_statevars_tave.F \
 do_the_model_io.F \
 do_write_pickup.F \
 dynamics.F \
 external_fields_load.F \
 external_forcing_surf.F \
 find_alpha.F \
 find_rho.F \
 forward_step.F \
 freesurf_rescale_g.F \
 freeze.F \
 freeze_surface.F \
 grad_sigma.F \
 impldiff.F \
 ini_cartesian_grid.F \
 ini_cg2d.F \
 ini_cg3d.F \
 ini_cori.F \
 ini_curvilinear_grid.F \
 ini_cylinder_grid.F \
 ini_depths.F \
 ini_dynvars.F \
 ini_eos.F \
 ini_fields.F \
 ini_forcing.F \
 ini_grid.F \
 ini_hfac.F \
 ini_linear_phisurf.F \
 ini_masks_etc.F \
 ini_mixing.F \
 ini_mnc_vars.F \
 ini_model_io.F \
 ini_nh_fields.F \
 ini_nh_vars.F \
 ini_nlfs_vars.F \
 ini_parms.F \
 ini_p_ground.F \
 ini_pressure.F \
 ini_psurf.F \
 ini_salt.F \
 ini_sigma_hfac.F \
 ini_spherical_polar_grid.F \
 ini_theta.F \
 initialise_fixed.F \
 initialise_varia.F \
 ini_vel.F \
 ini_vertical_grid.F \
 integrate_for_w.F \
 integr_continuity.F \
 load_fields_driver.F \
 load_grid_spacing.F \
 load_ref_files.F \
 modeldata_example.F \
 momentum_correction_step.F \
 packages_boot.F \
 packages_check.F \
 packages_error_msg.F \
 packages_init_fixed.F \
 packages_init_variables.F \
 packages_readparms.F \
 packages_write_pickup.F \
 plot_field.F \
 port_rand.F \
 post_cg3d.F \
 pre_cg3d.F \
 pressure_for_eos.F \
 read_pickup.F \
 remove_mean.F \
 rotate_spherical_polar_grid.F \
 rotate_uv2en.F \
 seawater.F \
 set_defaults.F \
 set_grid_factors.F \
 set_parms.F \
 set_ref_state.F \
 set_write_global_pickup.F \
 solve_for_pressure.F \
 solve_pentadiagonal.F \
 solve_tridiagonal.F \
 state_summary.F \
 swfrac.F \
 taueddy_external_forcing.F \
 taueddy_init_varia.F \
 the_main_loop.F \
 the_model_main.F \
 thermodynamics.F \
 timestep.F \
 timestep_tracer.F \
 timestep_wvel.F \
 tracers_correction_step.F \
 tracers_iigw_correction.F \
 turnoff_model_io.F \
 update_cg2d.F \
 update_etah.F \
 update_masks_etc.F \
 update_r_star.F \
 update_sigma.F \
 update_surf_dr.F \
 write_grid.F \
 write_pickup.F \
 write_state.F
NON_AD_F77_SRC_FILES = 
C_SRC_FILES =  \
 gsl_ieee_env.c \
 ptwrapper.c \
 setrlstk.c \
 sigreg.c \
 tim.c \
 timer_stats.c
F90_SRC_FILES = 
H_SRC_FILES =  \
 CPP_OPTIONS.h \
 OBCS_OPTIONS.h \
 PP81_DIAGS.h \
 PP81.h \
 SIZE.h \
 DEBUG_OPTIONS.h \
 GAD_FLUX_LIMITER.h \
 GAD.h \
 GAD_OPTIONS.h \
 GAD_SOM_VARS.h \
 MDSIO_BUFF_3D.h \
 MDSIO_BUFF_WH.h \
 MDSIO_OPTIONS.h \
 MOM_COMMON_OPTIONS.h \
 MOM_VISC.h \
 MOM_FLUXFORM.h \
 MOM_FLUXFORM_OPTIONS.h \
 MOM_VECINV_OPTIONS.h \
 MONITOR.h \
 MONITOR_OPTIONS.h \
 obcs_ad_check_lev1_dir.h \
 obcs_ad_check_lev2_dir.h \
 obcs_ad_check_lev3_dir.h \
 obcs_ad_check_lev4_dir.h \
 OBCS.h \
 OBCS_PTRACERS.h \
 ORLANSKI.h \
 PP81_OPTIONS.h \
 PP81_TAVE.h \
 RW_MFLDS.h \
 RW_OPTIONS.h \
 TIMEAVE_OPTIONS.h \
 TIMEAVE_STATV.h \
 ncvalues.h \
 netcdfcpp.h \
 netcdf.h \
 i_malloc.h \
 mpicxx.h \
 mpif.h \
 mpi.h \
 mpiof.h \
 mpio.h \
 BAR2.h \
 BARRIER.h \
 CPP_EEMACROS.h \
 CPP_EEOPTIONS.h \
 EEBUFF_SCPU.h \
 EEPARAMS.h \
 EESUPPORT.h \
 EXCH.h \
 GLOBAL_MAX.h \
 GLOBAL_SUM.h \
 MAIN_PDIRECTIVES1.h \
 MAIN_PDIRECTIVES2.h \
 MPI_INFO.h \
 SIGREG.h \
 CG2D.h \
 CG3D.h \
 DXC_MACROS.h \
 DXF_MACROS.h \
 DXG_MACROS.h \
 DXV_MACROS.h \
 DYC_MACROS.h \
 DYF_MACROS.h \
 DYG_MACROS.h \
 DYNVARS.h \
 DYU_MACROS.h \
 EOS.h \
 FCORI_MACROS.h \
 FFIELDS.h \
 GRID.h \
 GRID_MACROS.h \
 HFACC_MACROS.h \
 HFACS_MACROS.h \
 HFACW_MACROS.h \
 MASKS_MACROS.h \
 MASKW_MACROS.h \
 NH_VARS.h \
 PARAMS.h \
 PARAMS_MACROS.h \
 RA_MACROS.h \
 RAS_MACROS.h \
 RAW_MACROS.h \
 RECIP_DXC_MACROS.h \
 RECIP_DXF_MACROS.h \
 RECIP_DXG_MACROS.h \
 RECIP_DXV_MACROS.h \
 RECIP_DYC_MACROS.h \
 RECIP_DYF_MACROS.h \
 RECIP_DYG_MACROS.h \
 RECIP_DYU_MACROS.h \
 RECIP_HFACC_MACROS.h \
 RECIP_HFACS_MACROS.h \
 RECIP_HFACW_MACROS.h \
 RESTART.h \
 SOLVE_FOR_PRESSURE3D.h \
 SOLVE_FOR_PRESSURE.h \
 SURFACE.h \
 TANPHIATU_MACROS.h \
 TANPHIATV_MACROS.h \
 THE_MODEL_COMMON_BLOCKS.h \
 XC_MACROS.h \
 YC_MACROS.h
AD_FLOW_FILES =  \
 debug_ad.flow \
 mdsio_ad.flow \
 monitor_ad.flow \
 obcs_ad.flow \
 rw_ad.flow \
 eesupp.flow \
 exch1_ad.flow

F77_PP_SRC_FILES = $(F77_SRC_FILES:.F=.f)
F90_PP_SRC_FILES = $(F90_SRC_FILES:.F90=.f90)
OBJFILES= $(F77_SRC_FILES:.F=.o) $(C_SRC_FILES:.c=.o) $(F90_SRC_FILES:.F90=.o)
FLOFILES =  $(AD_FLOW_FILES:.flow=.flowdir)

.SUFFIXES:
.SUFFIXES: .o .f .p .F .c .ff90 .f90 .FF90 .F90 .flowdir .flow

all: $(EXECUTABLE)
$(EXECUTABLE): $(SPECIAL_FILES) $(F77_SRC_FILES) $(C_SRC_FILES) $(H_SRC_FILES) $(F90_SRC_FILES) $(OBJFILES) $(EMBEDDED_FILES)
	@echo Creating $@ ...
	$(LINK) -o $@ $(FFLAGS) $(FOPTIM) $(OBJFILES) $(LIBS)
depend:
	@make links
	$(MAKEDEPEND) -o .f $(DEFINES) $(INCLUDES) $(F77_SRC_FILES)
	$(TOOLSDIR)/f90mkdepend >> $(MAKEFILE)
	-rm -f makedepend.out

lib: libmitgcmuv.a

libmitgcmuv.a: $(OBJFILES)
	ar rcv libmitgcmuv.a $(OBJFILES)
	ar d   libmitgcmuv.a main.o

links: $(F77_SRC_FILES) $(C_SRC_FILES) $(H_SRC_FILES) $(F90_SRC_FILES) $(SPECIAL_FILES)

small_f: $(F77_PP_SRC_FILES) $(F90_PP_SRC_FILES)

output.txt: $(EXECUTABLE)
	@printf 'running ... '
	@$(EXECUTABLE) > $@

# remove most of the files that "make" generates
clean:
	-rm -rf *.p *.f90 *.mod  work.{pc,pcl} *.template
	-rm -rf *.o
	-rm -rf *.f *.flowdir
	-rm -rf *.ff90 $(AD_CLEAN) ad_input*

# remove most of the files that "make" and "make depend" generate
Clean:
	@make clean
	@make cleanlinks
	-rm -f $(SPECIAL_FILES) f90mkdepend.log
	-rm -f taf_command taf_output taf_ad.log taf_ad_flow.log
	-rm -f genmake_warnings genmake_errors make.log

# remove also the executable, files that "genmake2" generates (except Makefile)
#         and output from a run (plus log files from testreport)
CLEAN:
	@make Clean
	-rm -f $(EXECUTABLE) $(EXE_AD) *.bak Makefile.old
	-rm -f genmake.log genmake_state genmake_*optfile genmake.tr_log make.tr_log
	-find $(EXEDIR) -name "*.meta" -exec rm {} \;
	-find $(EXEDIR) -name "*.data" -exec rm {} \;
	-find $(EXEDIR) -name "fort.*" -exec rm {} \;
	-rm -f *.txt STD* *diagnostics.log datetime
	-rm -f *_MIT_CE_000.opt0000 costfunction*0000
	-rm -rf mnc_test_*

makefile:
	$(TOOLSDIR)/genmake2  "-rootdir=/model_output2/intwave/MITgcm_c62o" "-mods=../code" "-mpi" "-of=/model_output2/intwave/MITgcm_c62o/tools/build_options/linux_amd64_ifort_kraken" "-disable=mnc"
cleanlinks:
	-find . -type l -exec rm {} \;

# Special targets (SPECIAL_FILES) which are created by make
PACKAGES_CONFIG.h:
	@echo Creating $@ ...
	@$(TOOLSDIR)/convert_cpp_cmd2defines "Warning - this file is automatically generated - do NOT edit" -bPACKAGES_CONFIG_H "Disabled packages:" $(DISABLED_PACKAGES) " " "Enabled packages:" $(ENABLED_PACKAGES) > $@
AD_CONFIG.h:
	@echo Creating $@ ...
	@$(TOOLSDIR)/convert_cpp_cmd2defines "Warning - this file is automatically generated - do NOT edit" -bAD_CONFIG_H -UALLOW_ADJOINT_RUN -UALLOW_TANGENTLINEAR_RUN -UALLOW_ECCO_OPTIMIZATION > $@
FC_NAMEMANGLE.h:
	@echo Creating $@ ...
	echo "#define FC_NAMEMANGLE(X)  X ## _" > $@

BUILD_INFO.h:
	@echo Creating $@ ...
	-echo "#define THISVER 'checkpoint62o'" > $@
	-echo "#define THISUSER 'ecom'" >> $@
	-echo "#define THISDATE 'Fri Nov 28 17:24:16 CST 2014'" >> $@
	-echo "#define THISHOST 'kraken'" >> $@

# The normal chain of rules is (  .F - .f - .o  )

## This nullifies any default implicit rules concerning these two file types:
## %.o : %.F

.F.f:
	$(CPP) $(DEFINES) $(INCLUDES) > $@
.f.o:
	$(FC) $(FFLAGS) $(FOPTIM) -c $<
.F.o:
	$(FC) $(FFLAGS) $(FOPTIM) -c $<
.F90.f90:
	$(CPP) $(DEFINES) $(INCLUDES) > $@
.FF90.ff90:
	$(CPP) $(DEFINES) $(INCLUDES) > $@
.f90.o:
	$(F90C) $(F90FLAGS) $(F90OPTIM) -c $<
.ff90.o:
	cp $< $(basename  $<).f90
	$(F90C) $(F90FLAGS) $(F90OPTIM) -c $(F90FIXEDFORMAT) $(basename  $<).f90
.c.o:
	$(CC) $(CFLAGS) $(DEFINES) $(INCLUDES) -c $<
.flow.flowdir:
	$(CPP) $(DEFINES) $(INCLUDES) > $@

# Special exceptions that use the ( .F - .p - .f - .o ) rule-chain
.F.p:
	$(CPP) $(DEFINES) $(INCLUDES) > $@
.p.f:
	$(KPP) $(KFLAGS1)$@ $(KFLAGS2) $<


TAMC           = tamc
TAF            = staf

TAF_EXTRA      = 
TAMC_EXTRA     = 

AD_TAMC_FLAGS        = -reverse -admark ad -i4 -r4 -l tamc_ad.log -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_lwflux_dummy xx_lwdown_dummy xx_evap_dummy xx_snowprecip_dummy xx_apressure_dummy xx_runoff_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_depth_dummy xx_diffkr_dummy xx_kapgm_dummy xx_kapredi_dummy xx_bottomdrag_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_siarea_dummy xx_siheff_dummy xx_sihsnow_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'
AD_TAF_FLAGS         = -v1 -reverse -admark ad -i4 -r4 -l taf_ad.log -flow taf_ad_flow.log -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_lwflux_dummy xx_lwdown_dummy xx_evap_dummy xx_snowprecip_dummy xx_apressure_dummy xx_runoff_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_depth_dummy xx_diffkr_dummy xx_kapgm_dummy xx_kapredi_dummy xx_bottomdrag_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_siarea_dummy xx_siheff_dummy xx_sihsnow_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'
FTL_TAMC_FLAGS       = -forward -ftlmark g_ -i4 -r4 -l tamc_ftl.log -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_lwflux_dummy xx_lwdown_dummy xx_evap_dummy xx_snowprecip_dummy xx_apressure_dummy xx_runoff_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_depth_dummy xx_diffkr_dummy xx_kapgm_dummy xx_kapredi_dummy xx_bottomdrag_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_siarea_dummy xx_siheff_dummy xx_sihsnow_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'
FTL_TAF_FLAGS        = -v1 -forward -ftlmark g_ -i4 -r4 -l taf_ftl.log -flow taf_ftl_flow.log -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_lwflux_dummy xx_lwdown_dummy xx_evap_dummy xx_snowprecip_dummy xx_apressure_dummy xx_runoff_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_depth_dummy xx_diffkr_dummy xx_kapgm_dummy xx_kapredi_dummy xx_bottomdrag_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_siarea_dummy xx_siheff_dummy xx_sihsnow_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'
SVD_TAMC_FLAGS       = -reverse -forward -pure -i4 -r4 -l tamc_svd.log -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_lwflux_dummy xx_lwdown_dummy xx_evap_dummy xx_snowprecip_dummy xx_apressure_dummy xx_runoff_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_depth_dummy xx_diffkr_dummy xx_kapgm_dummy xx_kapredi_dummy xx_bottomdrag_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_siarea_dummy xx_siheff_dummy xx_sihsnow_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'
SVD_TAF_FLAGS        = -v1 -reverse -forward -pure -i4 -r4 -l taf_svd.log -flow taf_flow.log -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_lwflux_dummy xx_lwdown_dummy xx_evap_dummy xx_snowprecip_dummy xx_apressure_dummy xx_runoff_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_depth_dummy xx_diffkr_dummy xx_kapgm_dummy xx_kapredi_dummy xx_bottomdrag_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_siarea_dummy xx_siheff_dummy xx_sihsnow_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'

AD_FILES =  \
 gad_advection.f \
 gad_biharm_x.f \
 gad_biharm_y.f \
 gad_c2_adv_r.f \
 gad_c2_impl_r.f \
 gad_c2_adv_x.f \
 gad_c2_adv_y.f \
 gad_c4_adv_r.f \
 gad_c4_adv_x.f \
 gad_c4_adv_y.f \
 gad_calc_rhs.f \
 gad_del2.f \
 gad_diff_r.f \
 gad_diff_x.f \
 gad_diff_y.f \
 gad_dst2u1_adv_r.f \
 gad_dst2u1_impl_r.f \
 gad_dst2u1_adv_x.f \
 gad_dst2u1_adv_y.f \
 gad_dst3_adv_x.f \
 gad_dst3_adv_y.f \
 gad_dst3_adv_r.f \
 gad_dst3fl_adv_x.f \
 gad_dst3fl_adv_y.f \
 gad_dst3fl_adv_r.f \
 gad_dst3fl_impl_r.f \
 gad_fluxlimit_adv_r.f \
 gad_fluxlimit_impl_r.f \
 gad_fluxlimit_adv_x.f \
 gad_fluxlimit_adv_y.f \
 gad_grad_x.f \
 gad_grad_y.f \
 gad_implicit_r.f \
 gad_init_varia.f \
 gad_os7mp_adv_x.f \
 gad_os7mp_adv_y.f \
 gad_os7mp_adv_r.f \
 gad_u3_adv_r.f \
 gad_u3c4_impl_r.f \
 gad_u3_adv_x.f \
 gad_u3_adv_y.f \
 gad_som_exchanges.f \
 mom_calc_absvort3.f \
 mom_calc_hdiv.f \
 mom_calc_hfacz.f \
 mom_calc_ke.f \
 mom_calc_relvort3.f \
 mom_calc_strain.f \
 mom_calc_tension.f \
 mom_calc_visc.f \
 mom_hdissip.f \
 mom_quasihydrostatic.f \
 mom_u_coriolis_nh.f \
 mom_u_metric_nh.f \
 mom_u_bottomdrag.f \
 mom_u_rviscflux.f \
 mom_u_sidedrag.f \
 mom_v_coriolis_nh.f \
 mom_v_metric_nh.f \
 mom_v_bottomdrag.f \
 mom_v_rviscflux.f \
 mom_v_sidedrag.f \
 mom_calc_rtrans.f \
 mom_fluxform.f \
 mom_u_adv_uu.f \
 mom_u_adv_vu.f \
 mom_u_adv_wu.f \
 mom_u_coriolis.f \
 mom_u_del2u.f \
 mom_u_metric_cylinder.f \
 mom_u_metric_sphere.f \
 mom_u_xviscflux.f \
 mom_u_yviscflux.f \
 mom_v_adv_uv.f \
 mom_v_adv_vv.f \
 mom_v_adv_wv.f \
 mom_v_coriolis.f \
 mom_v_del2v.f \
 mom_v_metric_cylinder.f \
 mom_v_metric_sphere.f \
 mom_v_xviscflux.f \
 mom_v_yviscflux.f \
 mom_vecinv.f \
 mom_vi_coriolis.f \
 mom_vi_del2uv.f \
 mom_vi_hdissip.f \
 mom_vi_u_coriolis.f \
 mom_vi_u_coriolis_c4.f \
 mom_vi_u_grad_ke.f \
 mom_vi_u_vertshear.f \
 mom_vi_v_coriolis.f \
 mom_vi_v_coriolis_c4.f \
 mom_vi_v_grad_ke.f \
 mom_vi_v_vertshear.f \
 obcs_apply_eta.f \
 obcs_apply_seaice.f \
 obcs_apply_uvice.f \
 obcs_apply_r_star.f \
 obcs_apply_surf_dr.f \
 obcs_apply_ts.f \
 obcs_apply_uv.f \
 obcs_apply_w.f \
 obcs_apply_ptracer.f \
 obcs_calc.f \
 obcs_copy_tracer.f \
 obcs_init_variables.f \
 obcs_prescribe_exchanges.f \
 obcs_prescribe_read.f \
 obcs_sponge.f \
 orlanski_east.f \
 orlanski_north.f \
 orlanski_south.f \
 orlanski_west.f \
 orlanski_init.f \
 read_fld_xyz_rs.f \
 read_fld_xyz_rl.f \
 read_fld_xy_rs.f \
 read_fld_xy_rl.f \
 read_rec.f \
 timeave_cumul_1k1t.f \
 timeave_cumul_1t.f \
 timeave_cumul_1vfc.f \
 timeave_cumul_2v.f \
 timeave_cumul_2vfc.f \
 timeave_cumulate.f \
 timeave_cumul_dif_1t.f \
 timeave_norma_2v.f \
 timeave_normaliz.f \
 timeave_normalize.f \
 timeave_reset.f \
 timeave_statvars.f \
 timeave_statv_write.f \
 timeave_surf_flux.f \
 different_multiple.f \
 fill_cs_corner_tr_rl.f \
 fill_cs_corner_uv_rs.f \
 lef_zero.f \
 exch_3d_rs.f \
 exch_3d_rl.f \
 exch_xy_rl.f \
 exch_xy_rs.f \
 exch_xyz_rl.f \
 exch_xyz_rs.f \
 exch_uv_3d_rs.f \
 exch_uv_3d_rl.f \
 exch_uv_xy_rs.f \
 exch_uv_xy_rl.f \
 exch_uv_xyz_rs.f \
 exch_uv_xyz_rl.f \
 exch_uv_agrid_3d_rs.f \
 exch_uv_agrid_3d_rl.f \
 exch_uv_dgrid_3d_rs.f \
 exch_uv_dgrid_3d_rl.f \
 adams_bashforth2.f \
 adams_bashforth3.f \
 calc_3d_diffusivity.f \
 calc_buoyancy.f \
 calc_common_factors.f \
 calc_diffusivity.f \
 calc_div_ghat.f \
 calc_grad_phi_hyd.f \
 calc_grad_phi_surf.f \
 calc_gs.f \
 calc_gt.f \
 calc_ivdc.f \
 calc_phi_hyd.f \
 calc_surf_dr.f \
 calc_viscosity.f \
 calc_wsurf_tr.f \
 cg2d_nsa.f \
 convective_adjustment.f \
 convective_adjustment_ini.f \
 convective_weights.f \
 convectively_mixtracer.f \
 correction_step.f \
 cycle_tracer.f \
 diags_phi_hyd.f \
 diags_phi_rlow.f \
 diags_rho.f \
 do_atmospheric_phys.f \
 do_fields_blocking_exchanges.f \
 do_oceanic_phys.f \
 do_stagger_fields_exchanges.f \
 dynamics.f \
 external_fields_load.f \
 external_forcing.f \
 external_forcing_surf.f \
 find_alpha.f \
 find_rho.f \
 forward_step.f \
 freesurf_rescale_g.f \
 freeze.f \
 freeze_surface.f \
 grad_sigma.f \
 impldiff.f \
 ini_autodiff.f \
 ini_cartesian_grid.f \
 ini_cg2d.f \
 ini_cori.f \
 ini_curvilinear_grid.f \
 ini_cylinder_grid.f \
 ini_depths.f \
 ini_fields.f \
 ini_forcing.f \
 ini_grid.f \
 ini_hfac.f \
 ini_linear_phisurf.f \
 ini_masks_etc.f \
 ini_mixing.f \
 ini_nh_fields.f \
 ini_nlfs_vars.f \
 ini_p_ground.f \
 ini_pressure.f \
 ini_psurf.f \
 ini_salt.f \
 ini_sigma_hfac.f \
 ini_spherical_polar_grid.f \
 ini_theta.f \
 ini_dynvars.f \
 ini_vel.f \
 ini_vertical_grid.f \
 initialise_varia.f \
 integrate_for_w.f \
 integr_continuity.f \
 load_fields_driver.f \
 load_grid_spacing.f \
 momentum_correction_step.f \
 packages_init_variables.f \
 pressure_for_eos.f \
 read_pickup.f \
 rotate_uv2en.f \
 seawater.f \
 solve_for_pressure.f \
 state_summary.f \
 solve_pentadiagonal.f \
 solve_tridiagonal.f \
 taueddy_external_forcing.f \
 taueddy_init_varia.f \
 timestep.f \
 the_main_loop.f \
 thermodynamics.f \
 timestep_tracer.f \
 tracers_correction_step.f \
 update_cg2d.f \
 update_etah.f \
 update_masks_etc.f \
 update_surf_dr.f \
 swfrac.f \
 calc_r_star.f \
 update_r_star.f

# ... AD ...
adall: $(EXE_AD)
adtaf: ad_taf_output.f
adtamc: ad_tamc_output.f

ad_input_code.f: $(AD_FILES) $(H_SRC_FILES) $(AD_FLOW_FILES)
	@$(TOOLSDIR)/convert_cpp_cmd2defines "Warning - this file is automatically generated - do NOT edit" -DALLOW_ADJOINT_RUN -UALLOW_TANGENTLINEAR_RUN -UALLOW_ECCO_OPTIMIZATION > ad_config.template
	cmp ad_config.template AD_CONFIG.h || cat ad_config.template > AD_CONFIG.h
	-rm -f ad_config.template
	@make $(F77_PP_SRC_FILES)
	@make $(FLOFILES)
	cat $(FLOFILES) $(AD_FILES) | sed -f $(TOOLSDIR)/remove_comments_sed > ad_input_code.f

ad_taf_output.f: ad_input_code.f
	$(TAF) $(AD_TAF_FLAGS) $(TAF_EXTRA) ad_input_code.f
	ls -l ad_input_code_ad.f
	cat ad_input_code_ad.f | sed -f $(TOOLSDIR)/adjoint_sed > ad_taf_output.f

adtafonly:
	$(TAF) $(AD_TAF_FLAGS) $(TAF_EXTRA) ad_input_code.f
	ls -l ad_input_code_ad.f
	cat ad_input_code_ad.f | sed -f $(TOOLSDIR)/adjoint_sed > ad_taf_output.f

${EXE_AD}: ad_taf_output.o $(OBJFILES)
	$(LINK) -o ${EXE_AD} $(FFLAGS) $(FOPTIM) $(OBJFILES) ad_taf_output.o $(LIBS)

ad_tamc_output.f: ad_input_code.f
	$(TAMC) $(AD_TAMC_FLAGS) $(TAMC_EXTRA) ad_input_code.f
	cat ad_input_code_ad.f | sed -f $(TOOLSDIR)/adjoint_sed > ad_tamc_output.f

ad_tamc: ad_tamc_output.o $(OBJFILES)
	$(LINK) -o mitgcmuv_ad $(FFLAGS) $(FOPTIM) $(OBJFILES) ad_tamc_output.o $(LIBS)

adonlyfwd:
	patch < $(TOOLSDIR)/ad_taf_output.f.onlyfwd.diff

adtrick:
	patch < $(TOOLSDIR)/ad_taf_output.f.adtrick.diff

# ... FTL ...
ftlall: ftl_taf
ftltaf: ftl_taf_output.f
ftltamc: ftl_tamc_output.f

ftl_input_code.f: $(AD_FILES) $(H_SRC_FILES)
	@$(TOOLSDIR)/convert_cpp_cmd2defines "Warning - this file is automatically generated - do NOT edit" -UALLOW_ADJOINT_RUN -DALLOW_TANGENTLINEAR_RUN -UALLOW_ECCO_OPTIMIZATION > ftl_config.template
	cmp ftl_config.template AD_CONFIG.h || cat ftl_config.template > AD_CONFIG.h
	-rm -f ftl_config.template
	@make $(F77_PP_SRC_FILES)
	@make $(AD_FLOW_FILES)
	cat $(AD_FLOW_FILES) $(AD_FILES) > ftl_input_code.f

ftl_taf_output.f: ftl_input_code.f
	$(TAF) $(FTL_TAF_FLAGS) $(TAF_EXTRA) ftl_input_code.f
	ls -l ftl_input_code_ftl.f
	cat ftl_input_code_ftl.f | sed -f $(TOOLSDIR)/adjoint_sed > ftl_taf_output.f

ftltafonly:
	$(TAF) $(FTL_TAF_FLAGS) $(TAF_EXTRA) ftl_input_code.f
	ls -l ftl_input_code_ftl.f
	cat ftl_input_code_ftl.f | sed -f $(TOOLSDIR)/adjoint_sed > ftl_taf_output.f

ftl_taf: ftl_taf_output.o $(OBJFILES)
	$(LINK) -o mitgcmuv_ftl $(FFLAGS) $(FOPTIM) $(OBJFILES) ftl_taf_output.o $(LIBS)

ftl_tamc_output.f: ftl_input_code.f
	$(TAMC) $(FTL_TAMC_FLAGS) $(TAMC_EXTRA) ftl_input_code.f
	cat ftl_input_code_ftl.f | sed -f $(TOOLSDIR)/adjoint_sed > ftl_tamc_output.f

ftl_tamc: ftl_tamc_output.o $(OBJFILES)
	$(LINK) -o mitgcmuv_ftl $(FFLAGS) $(FOPTIM) $(OBJFILES) ftl_tamc_output.o $(LIBS)

# ... SVD ...
svdtaf: ad_taf_output.f ftl_taf_output.f
	@echo "--->>> Only ran TAF to generate SVD code!    <<<---"
	@echo "--->>> Do make svdall afterwards to compile. <<<---"
svdall: svd_touch svd_taf

svd_taf: $(OBJFILES)
	$(LINK) -o mitgcmuv_svd $(FFLAGS) $(FOPTIM) $(OBJFILES) ad_taf_output.o ftl_taf_output.o $(LIBS)

	@echo "--->>> Only COMPILE svd code! <<<---"
	@echo "--->>> Assumes you previously <<<---"
	@echo "--->>> did make svdtaf        <<<---"

svd_touch:
	@echo "--->>> Only COMPILE svd code! <<<---"
	@echo "--->>> Assumes you previously <<<---"
	@echo "--->>> did make svdtaf        <<<---"
	touch ad_taf_output.f ftl_taf_output.f
	$(FC) $(FFLAGS) $(FOPTIM) -c ad_taf_output.f
	$(FC) $(FFLAGS) $(FOPTIM) -c ftl_taf_output.f
	@$(TOOLSDIR)/convert_cpp_cmd2defines "Warning - this file is automatically generated - do NOT edit" -UALLOW_ADJOINT_RUN -DALLOW_TANGENTLINEAR_RUN -UALLOW_ECCO_OPTIMIZATION > ftl_config.template
	cmp ftl_config.template AD_CONFIG.h || cat ftl_config.template > AD_CONFIG.h
	-rm -f ftl_config.template

# This section creates symbolic links


#  These files are linked from ../code
 CPP_OPTIONS.h external_forcing.F OBCS_OPTIONS.h pp81_calc.F PP81_DIAGS.h pp81_do_diags.F PP81.h pp81_output.F SIZE.h :
	if [ ! -L $@ ]; then $(LN) ../code/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/pkg/debug
 chksum_tiled.F debug_call.F debug_cs_corner_uv.F debug_enter.F debug_fld_stats_rl.F debug_fld_stats_rs.F debug_leave.F debug_msg.F DEBUG_OPTIONS.h debug_stats_rl.F debug_stats_rs.F fill_in_corners_rl.F write_fullarray_rl.F debug_ad.flow :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/pkg/debug/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/pkg/generic_advdiff
 gad_advection.F gad_biharm_x.F gad_biharm_y.F gad_c2_adv_r.F gad_c2_adv_x.F gad_c2_adv_y.F gad_c2_impl_r.F gad_c4_adv_r.F gad_c4_adv_x.F gad_c4_adv_y.F gad_calc_rhs.F gad_check.F gad_del2.F gad_diagnostics_init.F gad_diagnostics_state.F gad_diff_r.F gad_diff_x.F gad_diff_y.F gad_dst2u1_adv_r.F gad_dst2u1_adv_x.F gad_dst2u1_adv_y.F gad_dst2u1_impl_r.F gad_dst3_adv_r.F gad_dst3_adv_x.F gad_dst3_adv_y.F gad_dst3fl_adv_r.F gad_dst3fl_adv_x.F gad_dst3fl_adv_y.F gad_dst3fl_impl_r.F gad_exch_som.F gad_fluxlimit_adv_r.F gad_fluxlimit_adv_x.F gad_fluxlimit_adv_y.F GAD_FLUX_LIMITER.h gad_fluxlimit_impl_r.F gad_grad_x.F gad_grad_y.F GAD.h gad_implicit_r.F gad_init_fixed.F gad_init_varia.F GAD_OPTIONS.h gad_os7mp_adv_r.F gad_os7mp_adv_x.F gad_os7mp_adv_y.F gad_read_pickup.F gad_som_advect.F gad_som_adv_r.F gad_som_adv_x.F gad_som_adv_y.F gad_som_exchanges.F gad_som_fill_cs_corner.F gad_som_lim_r.F gad_som_prep_cs_corner.F GAD_SOM_VARS.h gad_u3_adv_r.F gad_u3_adv_x.F gad_u3_adv_y.F gad_u3c4_impl_r.F gad_valid_advscheme.F gad_write_pickup.F salt_fill.F :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/pkg/generic_advdiff/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/pkg/mdsio
 MDSIO_BUFF_3D.h mdsio_buffertorl.F mdsio_buffertors.F MDSIO_BUFF_WH.h mdsio_facef_read.F mdsio_gl.F mdsio_gl_slice.F MDSIO_OPTIONS.h mdsio_pass_r4torl.F mdsio_pass_r4tors.F mdsio_pass_r8torl.F mdsio_pass_r8tors.F mdsio_pass_wh_r4torl.F mdsio_pass_wh_r4tors.F mdsio_pass_wh_r8torl.F mdsio_pass_wh_r8tors.F mdsio_rd_rec_rl.F mdsio_rd_rec_rs.F mdsio_read_field.F mdsio_read_meta.F mdsio_read_section.F mdsio_readvector.F mdsio_read_whalos.F mdsio_rw_field.F mdsio_rw_slice.F mdsio_seg4torl.F mdsio_seg4tors.F mdsio_seg8torl.F mdsio_seg8tors.F mdsio_segxtorx_2d.F mdsio_write_field.F mdsio_writelocal.F mdsio_write_meta.F mdsio_write_section.F mdsio_writevec_loc.F mdsio_writevector.F mdsio_write_whalos.F mdsio_wr_metafiles.F mdsio_wr_rec_rl.F mdsio_wr_rec_rs.F mdsio_ad.flow :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/pkg/mdsio/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/pkg/mom_common
 mom_calc_absvort3.F mom_calc_hdiv.F mom_calc_hfacz.F mom_calc_ke.F mom_calc_relvort3.F mom_calc_strain.F mom_calc_tension.F mom_calc_visc.F MOM_COMMON_OPTIONS.h mom_diagnostics_init.F mom_hdissip.F mom_init_fixed.F mom_quasihydrostatic.F mom_u_bottomdrag.F mom_u_coriolis_nh.F mom_u_implicit_r.F mom_u_metric_nh.F mom_u_rviscflux.F mom_u_sidedrag.F mom_v_bottomdrag.F mom_v_coriolis_nh.F mom_v_implicit_r.F MOM_VISC.h mom_v_metric_nh.F mom_v_rviscflux.F mom_v_sidedrag.F mom_w_coriolis_nh.F mom_w_metric_nh.F mom_w_sidedrag.F :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/pkg/mom_common/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/pkg/mom_fluxform
 mom_calc_rtrans.F mom_fluxform.F MOM_FLUXFORM.h MOM_FLUXFORM_OPTIONS.h mom_u_adv_uu.F mom_u_adv_vu.F mom_u_adv_wu.F mom_u_coriolis.F mom_u_del2u.F mom_u_metric_cylinder.F mom_u_metric_sphere.F mom_uv_boundary.F mom_u_xviscflux.F mom_u_yviscflux.F mom_v_adv_uv.F mom_v_adv_vv.F mom_v_adv_wv.F mom_v_coriolis.F mom_v_del2v.F mom_v_metric_cylinder.F mom_v_metric_sphere.F mom_v_xviscflux.F mom_v_yviscflux.F :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/pkg/mom_fluxform/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/pkg/mom_vecinv
 mom_vecinv.F MOM_VECINV_OPTIONS.h mom_vi_coriolis.F mom_vi_del2uv.F mom_vi_hdissip.F mom_vi_u_coriolis_c4.F mom_vi_u_coriolis.F mom_vi_u_grad_ke.F mom_vi_u_vertshear.F mom_vi_v_coriolis_c4.F mom_vi_v_coriolis.F mom_vi_v_grad_ke.F mom_vi_v_vertshear.F :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/pkg/mom_vecinv/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/pkg/monitor
 mon_advcfl.F mon_advcflw2.F mon_advcflw.F mon_calc_stats_rl.F mon_calc_stats_rs.F mon_init.F monitor_ad.F monitor.F monitor_g.F MONITOR.h MONITOR_OPTIONS.h mon_ke.F mon_out.F mon_printstats_rl.F mon_printstats_rs.F mon_set_iounit.F mon_set_pref.F mon_solution.F mon_stats_latbnd_rl.F mon_stats_rl.F mon_stats_rs.F mon_surfcor.F mon_vort3.F mon_writestats_rl.F mon_writestats_rs.F monitor_ad.flow :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/pkg/monitor/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/pkg/obcs
 obcs_ad_check_lev1_dir.h obcs_ad_check_lev2_dir.h obcs_ad_check_lev3_dir.h obcs_ad_check_lev4_dir.h obcs_apply_eta.F obcs_apply_ptracer.F obcs_apply_r_star.F obcs_apply_seaice.F obcs_apply_sloc.F obcs_apply_surf_dr.F obcs_apply_tloc.F obcs_apply_ts.F obcs_apply_uv.F obcs_apply_uvice.F obcs_apply_w.F obcs_calc.F obcs_calc_stevens.F obcs_check_depths.F obcs_check.F obcs_copy_tracer.F obcs_external_fields_load.F OBCS.h obcs_init_fixed.F obcs_init_variables.F obcs_output.F obcs_prescribe_exchanges.F obcs_prescribe_read.F OBCS_PTRACERS.h obcs_readparms.F obcs_read_pickup.F obcs_sponge.F obcs_write_pickup.F orlanski_east.F ORLANSKI.h orlanski_init.F orlanski_north.F orlanski_south.F orlanski_west.F obcs_ad.flow :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/pkg/obcs/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/pkg/pp81
 pp81_calc_diff.F pp81_calc_visc.F pp81_check.F pp81_init_varia.F PP81_OPTIONS.h pp81_readparms.F pp81_ri_number.F PP81_TAVE.h :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/pkg/pp81/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/pkg/rw
 get_write_global_fld.F read_fld_xy_rl.F read_fld_xy_rs.F read_fld_xyz_rl.F read_fld_xyz_rs.F read_mflds.F read_rec.F RW_MFLDS.h RW_OPTIONS.h set_write_global_fld.F write_fld_3d_rl.F write_fld_3d_rs.F write_fld_s3d_rl.F write_fld_s3d_rs.F write_fld_xy_rl.F write_fld_xy_rs.F write_fld_xyz_rl.F write_fld_xyz_rs.F write_glvec_rl.F write_glvec_rs.F write_local_rl.F write_local_rs.F write_rec.F rw_ad.flow :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/pkg/rw/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/pkg/timeave
 timeave_cumul_1k1t.F timeave_cumul_1t.F timeave_cumul_1vfc.F timeave_cumul_2v.F timeave_cumul_2vfc.F timeave_cumulate.F timeave_cumul_dif_1t.F timeave_init_fixed.F timeave_norma_2v.F timeave_normalize.F timeave_normaliz.F TIMEAVE_OPTIONS.h timeave_reset.F timeave_statvars.F TIMEAVE_STATV.h timeave_statv_write.F timeave_surf_flux.F :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/pkg/timeave/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/eesupp/src
 all_proc_die.F bar2.F bar_check.F barrier.F check_threads.F comm_stats.F different_multiple.F diff_phase_multiple.F eeboot.F eeboot_minimal.F eedata_example.F eedie.F eeintro_msg.F eeset_parms.F eewrite_eeenv.F exch1_bg_r4_cube.F exch1_bg_r8_cube.F exch1_bg_rl_cube.F exch1_bg_rs_cube.F exch1_r4_cube.F exch1_r4.F exch1_r8_cube.F exch1_r8.F exch1_rl_ad.F exch1_rl_cube_ad.F exch1_rl_cube.F exch1_rl.F exch1_rs_ad.F exch1_rs_cube_ad.F exch1_rs_cube.F exch1_rs.F exch1_uv_r4_cube.F exch1_uv_r8_cube.F exch1_uv_rl_cube.F exch1_uv_rs_cube.F exch1_z_r4_cube.F exch1_z_r8_cube.F exch1_z_rl_cube.F exch1_z_rs_cube.F exch_3d_r4.F exch_3d_r8.F exch_3d_rl.F exch_3d_rs.F exch_cycle_ebl.F exch_init.F exch_r4_recv_get_x.F exch_r4_recv_get_y.F exch_r4_send_put_x.F exch_r4_send_put_y.F exch_r8_recv_get_x.F exch_r8_recv_get_y.F exch_r8_send_put_x.F exch_r8_send_put_y.F exch_rl_recv_get_x.F exch_rl_recv_get_y.F exch_rl_send_put_x.F exch_rl_send_put_y.F exch_rs_recv_get_x.F exch_rs_recv_get_y.F exch_rs_send_put_x.F exch_rs_send_put_y.F exch_s3d_r4.F exch_s3d_r8.F exch_s3d_rl.F exch_s3d_rs.F exch_sm_3d_r4.F exch_sm_3d_r8.F exch_sm_3d_rl.F exch_sm_3d_rs.F exch_uv_3d_r4.F exch_uv_3d_r8.F exch_uv_3d_rl.F exch_uv_3d_rs.F exch_uv_agrid_3d_r4.F exch_uv_agrid_3d_r8.F exch_uv_agrid_3d_rl.F exch_uv_agrid_3d_rs.F exch_uv_bgrid_3d_r4.F exch_uv_bgrid_3d_r8.F exch_uv_bgrid_3d_rl.F exch_uv_bgrid_3d_rs.F exch_uv_dgrid_3d_r4.F exch_uv_dgrid_3d_r8.F exch_uv_dgrid_3d_rl.F exch_uv_dgrid_3d_rs.F exch_uv_xy_r4.F exch_uv_xy_r8.F exch_uv_xy_rl.F exch_uv_xy_rs.F exch_uv_xyz_r4.F exch_uv_xyz_r8.F exch_uv_xyz_rl.F exch_uv_xyz_rs.F exch_xy_r4.F exch_xy_r8.F exch_xy_rl.F exch_xy_rs.F exch_xyz_r4.F exch_xyz_r8.F exch_xyz_rl.F exch_xyz_rs.F exch_z_3d_r4.F exch_z_3d_r8.F exch_z_3d_rl.F exch_z_3d_rs.F fill_cs_corner_ag_rl.F fill_cs_corner_tr_rl.F fill_cs_corner_uv_rs.F fool_the_compiler.F gather_2d_r4.F gather_2d_r8.F gather_2d_wh_r4.F gather_2d_wh_r8.F gather_vector.F gather_xz.F gather_yz.F global_max.F global_sum.F global_sum_singlecpu.F global_sum_tile.F global_vec_sum.F gsl_ieee_env.c gsum.F ini_communication_patterns.F ini_procs.F ini_threading_environment.F lef_zero.F main.F master_cpu_io.F master_cpu_thread.F mds_byteswapi4.F mds_byteswapr4.F mds_byteswapr8.F mds_check4file.F mdsfindunit.F mds_reclen.F memsync.F nml_filter.F nml_set_terminator.F open_copy_data_file.F print.F ptwrapper.c scatter_2d_r4.F scatter_2d_r8.F scatter_2d_wh_r4.F scatter_2d_wh_r8.F scatter_vector.F scatter_xz.F scatter_yz.F setrlstk.c sigreg.c stop_if_error.F tim.c timers.F timer_stats.c utils.F write_utils.F eesupp.flow exch1_ad.flow :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/eesupp/src/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/model/src
 adams_bashforth2.F adams_bashforth3.F calc_3d_diffusivity.F calc_buoyancy.F calc_common_factors.F calc_diffusivity.F calc_div_ghat.F calc_grad_phi_hyd.F calc_grad_phi_surf.F calc_gs.F calc_gt.F calc_gw.F calc_ivdc.F calc_oce_mxlayer.F calc_phi_hyd.F calc_r_star.F calc_surf_dr.F calc_viscosity.F calc_wsurf_tr.F cg2d.F cg2d_nsa.F cg2d_sr.F cg3d.F check_pickup.F config_check.F config_summary.F convective_adjustment.F convective_adjustment_ini.F convectively_mixtracer.F convective_weights.F correction_step.F cycle_tracer.F diags_oceanic_surf_flux.F diags_phi_hyd.F diags_phi_rlow.F diags_rho.F do_atmospheric_phys.F do_fields_blocking_exchanges.F do_gterm_blocking_exchanges.F do_oceanic_phys.F do_stagger_fields_exchanges.F do_statevars_diags.F do_statevars_tave.F do_the_model_io.F do_write_pickup.F dynamics.F external_fields_load.F external_forcing_surf.F find_alpha.F find_rho.F forward_step.F freesurf_rescale_g.F freeze.F freeze_surface.F grad_sigma.F impldiff.F ini_cartesian_grid.F ini_cg2d.F ini_cg3d.F ini_cori.F ini_curvilinear_grid.F ini_cylinder_grid.F ini_depths.F ini_dynvars.F ini_eos.F ini_fields.F ini_forcing.F ini_grid.F ini_hfac.F ini_linear_phisurf.F ini_masks_etc.F ini_mixing.F ini_mnc_vars.F ini_model_io.F ini_nh_fields.F ini_nh_vars.F ini_nlfs_vars.F ini_parms.F ini_p_ground.F ini_pressure.F ini_psurf.F ini_salt.F ini_sigma_hfac.F ini_spherical_polar_grid.F ini_theta.F initialise_fixed.F initialise_varia.F ini_vel.F ini_vertical_grid.F integrate_for_w.F integr_continuity.F load_fields_driver.F load_grid_spacing.F load_ref_files.F modeldata_example.F momentum_correction_step.F packages_boot.F packages_check.F packages_error_msg.F packages_init_fixed.F packages_init_variables.F packages_readparms.F packages_write_pickup.F plot_field.F port_rand.F post_cg3d.F pre_cg3d.F pressure_for_eos.F read_pickup.F remove_mean.F rotate_spherical_polar_grid.F rotate_uv2en.F seawater.F set_defaults.F set_grid_factors.F set_parms.F set_ref_state.F set_write_global_pickup.F solve_for_pressure.F solve_pentadiagonal.F solve_tridiagonal.F state_summary.F swfrac.F taueddy_external_forcing.F taueddy_init_varia.F the_main_loop.F the_model_main.F thermodynamics.F timestep.F timestep_tracer.F timestep_wvel.F tracers_correction_step.F tracers_iigw_correction.F turnoff_model_io.F update_cg2d.F update_etah.F update_masks_etc.F update_r_star.F update_sigma.F update_surf_dr.F write_grid.F write_pickup.F write_state.F :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/model/src/$@ $@; fi

#  These files are linked from /shared/netcdf-4.1.2-intel//include
 ncvalues.h netcdfcpp.h netcdf.h :
	if [ ! -L $@ ]; then $(LN) /shared/netcdf-4.1.2-intel//include/$@ $@; fi

#  These files are linked from /shared/intel/impi/4.0.2.003/intel64/include
 i_malloc.h mpicxx.h mpif.h mpi.h mpiof.h mpio.h :
	if [ ! -L $@ ]; then $(LN) /shared/intel/impi/4.0.2.003/intel64/include/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/eesupp/inc
 BAR2.h BARRIER.h CPP_EEMACROS.h CPP_EEOPTIONS.h EEBUFF_SCPU.h EEPARAMS.h EESUPPORT.h EXCH.h GLOBAL_MAX.h GLOBAL_SUM.h MAIN_PDIRECTIVES1.h MAIN_PDIRECTIVES2.h MPI_INFO.h SIGREG.h :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/eesupp/inc/$@ $@; fi

#  These files are linked from /model_output2/intwave/MITgcm_c62o/model/inc
 CG2D.h CG3D.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYNVARS.h DYU_MACROS.h EOS.h FCORI_MACROS.h FFIELDS.h GRID.h GRID_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h MASKS_MACROS.h MASKW_MACROS.h NH_VARS.h PARAMS.h PARAMS_MACROS.h RA_MACROS.h RAS_MACROS.h RAW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h RESTART.h SOLVE_FOR_PRESSURE3D.h SOLVE_FOR_PRESSURE.h SURFACE.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h THE_MODEL_COMMON_BLOCKS.h XC_MACROS.h YC_MACROS.h :
	if [ ! -L $@ ]; then $(LN) /model_output2/intwave/MITgcm_c62o/model/inc/$@ $@; fi


# DO NOT DELETE

external_forcing.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
external_forcing.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
external_forcing.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
external_forcing.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
external_forcing.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
external_forcing.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
external_forcing.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
external_forcing.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
external_forcing.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
external_forcing.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
external_forcing.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
external_forcing.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
external_forcing.f: TANPHIATV_MACROS.h DYNVARS.h FFIELDS.h SURFACE.h
pp81_calc.f: PP81_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
pp81_calc.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
pp81_calc.f: FCORI_MACROS.h EOS.h DYNVARS.h PP81.h FFIELDS.h GRID.h
pp81_calc.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
pp81_calc.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
pp81_calc.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
pp81_calc.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
pp81_calc.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
pp81_calc.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
pp81_calc.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
pp81_calc.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
pp81_calc.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
pp81_do_diags.f: PP81_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
pp81_do_diags.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
pp81_do_diags.f: PARAMS_MACROS.h FCORI_MACROS.h PP81.h PP81_DIAGS.h
pp81_output.f: PP81_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
pp81_output.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
pp81_output.f: FCORI_MACROS.h PP81.h PP81_TAVE.h
chksum_tiled.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
chksum_tiled.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
debug_call.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
debug_call.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h
debug_cs_corner_uv.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_cs_corner_uv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
debug_enter.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_enter.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
debug_fld_stats_rl.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_fld_stats_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
debug_fld_stats_rs.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_fld_stats_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
debug_leave.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_leave.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
debug_msg.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
debug_msg.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h
debug_stats_rl.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_stats_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
debug_stats_rs.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_stats_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
fill_in_corners_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
write_fullarray_rl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
write_fullarray_rl.f: EEPARAMS.h
gad_advection.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_advection.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
gad_advection.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
gad_advection.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_advection.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_advection.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_advection.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_advection.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_advection.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_advection.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_advection.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_advection.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_advection.f: TANPHIATV_MACROS.h GAD.h
gad_biharm_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_biharm_x.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_biharm_x.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_biharm_x.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_biharm_x.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_biharm_x.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_biharm_x.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_biharm_x.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_biharm_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_biharm_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_biharm_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_biharm_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_biharm_y.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_biharm_y.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_biharm_y.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_biharm_y.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_biharm_y.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_biharm_y.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_biharm_y.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_biharm_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_biharm_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_biharm_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_c2_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c2_adv_r.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_c2_adv_r.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_c2_adv_r.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_c2_adv_r.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_c2_adv_r.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_c2_adv_r.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_c2_adv_r.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c2_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c2_adv_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c2_adv_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h
gad_c2_adv_r.f: PARAMS_MACROS.h FCORI_MACROS.h
gad_c2_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c2_adv_x.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_c2_adv_x.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_c2_adv_x.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_c2_adv_x.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_c2_adv_x.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_c2_adv_x.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_c2_adv_x.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c2_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c2_adv_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c2_adv_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_c2_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c2_adv_y.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_c2_adv_y.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_c2_adv_y.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_c2_adv_y.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_c2_adv_y.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_c2_adv_y.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_c2_adv_y.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c2_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c2_adv_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c2_adv_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_c2_impl_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_c2_impl_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
gad_c2_impl_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_c2_impl_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_c2_impl_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_c2_impl_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_c2_impl_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_c2_impl_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_c2_impl_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_c2_impl_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_c2_impl_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_c2_impl_r.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_c2_impl_r.f: FCORI_MACROS.h
gad_c4_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c4_adv_r.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_c4_adv_r.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_c4_adv_r.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_c4_adv_r.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_c4_adv_r.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_c4_adv_r.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_c4_adv_r.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c4_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c4_adv_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c4_adv_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_c4_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c4_adv_x.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_c4_adv_x.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_c4_adv_x.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_c4_adv_x.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_c4_adv_x.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_c4_adv_x.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_c4_adv_x.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c4_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c4_adv_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c4_adv_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_c4_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c4_adv_y.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_c4_adv_y.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_c4_adv_y.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_c4_adv_y.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_c4_adv_y.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_c4_adv_y.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_c4_adv_y.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c4_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c4_adv_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c4_adv_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_calc_rhs.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_calc_rhs.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_calc_rhs.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_calc_rhs.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_calc_rhs.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_calc_rhs.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_calc_rhs.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_calc_rhs.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_calc_rhs.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_calc_rhs.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_calc_rhs.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_calc_rhs.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h GAD.h
gad_check.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_check.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_check.f: FCORI_MACROS.h GAD.h
gad_del2.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_del2.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_del2.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_del2.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_del2.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_del2.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_del2.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_del2.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
gad_del2.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_del2.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_del2.f: TANPHIATV_MACROS.h
gad_diagnostics_init.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_diagnostics_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
gad_diagnostics_init.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GAD.h
gad_diagnostics_state.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_diagnostics_state.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
gad_diagnostics_state.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
gad_diagnostics_state.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_diagnostics_state.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_diagnostics_state.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_diagnostics_state.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_diagnostics_state.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_diagnostics_state.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_diagnostics_state.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_diagnostics_state.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_diagnostics_state.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
gad_diagnostics_state.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
gad_diagnostics_state.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_diagnostics_state.f: GAD.h GAD_SOM_VARS.h
gad_diff_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_diff_r.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_diff_r.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_diff_r.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_diff_r.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_diff_r.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_diff_r.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_diff_r.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_diff_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_diff_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_diff_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h
gad_diff_r.f: PARAMS_MACROS.h FCORI_MACROS.h
gad_diff_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_diff_x.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_diff_x.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_diff_x.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_diff_x.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_diff_x.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_diff_x.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_diff_x.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_diff_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_diff_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_diff_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_diff_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_diff_y.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_diff_y.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_diff_y.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_diff_y.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_diff_y.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_diff_y.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_diff_y.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_diff_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_diff_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_diff_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_dst2u1_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst2u1_adv_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_dst2u1_adv_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst2u1_adv_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst2u1_adv_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst2u1_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst2u1_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst2u1_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst2u1_adv_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst2u1_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst2u1_adv_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst2u1_adv_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst2u1_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst2u1_adv_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_dst2u1_adv_x.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst2u1_adv_x.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst2u1_adv_x.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst2u1_adv_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst2u1_adv_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst2u1_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst2u1_adv_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst2u1_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst2u1_adv_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst2u1_adv_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst2u1_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst2u1_adv_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_dst2u1_adv_y.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst2u1_adv_y.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst2u1_adv_y.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst2u1_adv_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst2u1_adv_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst2u1_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst2u1_adv_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst2u1_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst2u1_adv_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst2u1_adv_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst2u1_impl_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst2u1_impl_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_dst2u1_impl_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst2u1_impl_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst2u1_impl_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_dst2u1_impl_r.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_dst2u1_impl_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_dst2u1_impl_r.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst2u1_impl_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst2u1_impl_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst2u1_impl_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst2u1_impl_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst3_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3_adv_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
gad_dst3_adv_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_dst3_adv_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_dst3_adv_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_dst3_adv_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_dst3_adv_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_dst3_adv_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_dst3_adv_r.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_dst3_adv_r.f: FCORI_MACROS.h GAD.h
gad_dst3_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3_adv_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
gad_dst3_adv_x.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_dst3_adv_x.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_dst3_adv_x.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3_adv_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3_adv_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_dst3_adv_x.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_dst3_adv_x.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_dst3_adv_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_dst3_adv_x.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_dst3_adv_x.f: FCORI_MACROS.h GAD.h
gad_dst3_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3_adv_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
gad_dst3_adv_y.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_dst3_adv_y.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_dst3_adv_y.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3_adv_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3_adv_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_dst3_adv_y.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_dst3_adv_y.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_dst3_adv_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_dst3_adv_y.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_dst3_adv_y.f: FCORI_MACROS.h GAD.h
gad_dst3fl_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3fl_adv_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_dst3fl_adv_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst3fl_adv_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst3fl_adv_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3fl_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3fl_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3fl_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst3fl_adv_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst3fl_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst3fl_adv_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst3fl_adv_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst3fl_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3fl_adv_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_dst3fl_adv_x.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst3fl_adv_x.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst3fl_adv_x.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3fl_adv_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3fl_adv_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3fl_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst3fl_adv_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst3fl_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst3fl_adv_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst3fl_adv_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst3fl_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3fl_adv_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_dst3fl_adv_y.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst3fl_adv_y.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst3fl_adv_y.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3fl_adv_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3fl_adv_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3fl_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst3fl_adv_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst3fl_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst3fl_adv_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst3fl_adv_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst3fl_impl_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3fl_impl_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_dst3fl_impl_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst3fl_impl_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst3fl_impl_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_dst3fl_impl_r.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_dst3fl_impl_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_dst3fl_impl_r.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst3fl_impl_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst3fl_impl_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst3fl_impl_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst3fl_impl_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h EEPARAMS.h
gad_dst3fl_impl_r.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GAD.h
gad_exch_som.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_exch_som.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h GAD.h
gad_fluxlimit_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_fluxlimit_adv_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_fluxlimit_adv_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_fluxlimit_adv_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_fluxlimit_adv_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_fluxlimit_adv_r.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
gad_fluxlimit_adv_r.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
gad_fluxlimit_adv_r.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_fluxlimit_adv_r.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
gad_fluxlimit_adv_r.f: GAD_FLUX_LIMITER.h
gad_fluxlimit_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_fluxlimit_adv_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_fluxlimit_adv_x.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_fluxlimit_adv_x.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_fluxlimit_adv_x.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_fluxlimit_adv_x.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
gad_fluxlimit_adv_x.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
gad_fluxlimit_adv_x.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_fluxlimit_adv_x.f: GAD_FLUX_LIMITER.h
gad_fluxlimit_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_fluxlimit_adv_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_fluxlimit_adv_y.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_fluxlimit_adv_y.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_fluxlimit_adv_y.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_fluxlimit_adv_y.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
gad_fluxlimit_adv_y.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
gad_fluxlimit_adv_y.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_fluxlimit_adv_y.f: GAD_FLUX_LIMITER.h
gad_fluxlimit_impl_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_fluxlimit_impl_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
gad_fluxlimit_impl_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_fluxlimit_impl_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_fluxlimit_impl_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_fluxlimit_impl_r.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_fluxlimit_impl_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_fluxlimit_impl_r.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_fluxlimit_impl_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_fluxlimit_impl_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_fluxlimit_impl_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
gad_fluxlimit_impl_r.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
gad_fluxlimit_impl_r.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_fluxlimit_impl_r.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
gad_fluxlimit_impl_r.f: GAD_FLUX_LIMITER.h
gad_grad_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_grad_x.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h GRID.h GRID_MACROS.h
gad_grad_x.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_grad_x.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_grad_x.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_grad_x.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_grad_x.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_grad_x.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_grad_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_grad_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_grad_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_grad_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_grad_y.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h GRID.h GRID_MACROS.h
gad_grad_y.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_grad_y.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_grad_y.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_grad_y.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_grad_y.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_grad_y.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_grad_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_grad_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_grad_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_implicit_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_implicit_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
gad_implicit_r.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
gad_implicit_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_implicit_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_implicit_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_implicit_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_implicit_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_implicit_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_implicit_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_implicit_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_implicit_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_implicit_r.f: TANPHIATV_MACROS.h GAD.h
gad_init_fixed.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_init_fixed.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
gad_init_fixed.f: PARAMS_MACROS.h FCORI_MACROS.h GAD.h
gad_init_varia.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_init_varia.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
gad_init_varia.f: PARAMS_MACROS.h FCORI_MACROS.h GAD.h GAD_SOM_VARS.h
gad_os7mp_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_os7mp_adv_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
gad_os7mp_adv_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_os7mp_adv_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_os7mp_adv_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_os7mp_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_os7mp_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_os7mp_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_os7mp_adv_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_os7mp_adv_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_os7mp_adv_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_os7mp_adv_r.f: TANPHIATV_MACROS.h GAD.h
gad_os7mp_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_os7mp_adv_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
gad_os7mp_adv_x.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_os7mp_adv_x.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_os7mp_adv_x.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_os7mp_adv_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_os7mp_adv_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_os7mp_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_os7mp_adv_x.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_os7mp_adv_x.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_os7mp_adv_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_os7mp_adv_x.f: TANPHIATV_MACROS.h GAD.h
gad_os7mp_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_os7mp_adv_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
gad_os7mp_adv_y.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_os7mp_adv_y.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_os7mp_adv_y.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_os7mp_adv_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_os7mp_adv_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_os7mp_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_os7mp_adv_y.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_os7mp_adv_y.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_os7mp_adv_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_os7mp_adv_y.f: TANPHIATV_MACROS.h GAD.h
gad_read_pickup.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_read_pickup.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
gad_read_pickup.f: PARAMS_MACROS.h FCORI_MACROS.h GAD.h GAD_SOM_VARS.h
gad_som_advect.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_som_advect.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
gad_som_advect.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
gad_som_advect.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_som_advect.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_som_advect.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_som_advect.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_som_advect.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_som_advect.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_som_advect.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_som_advect.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_som_advect.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_som_advect.f: TANPHIATV_MACROS.h GAD.h
gad_som_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_som_adv_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
gad_som_adv_r.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
gad_som_adv_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_som_adv_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_som_adv_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_som_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_som_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_som_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_som_adv_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_som_adv_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_som_adv_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_som_adv_r.f: TANPHIATV_MACROS.h GAD.h
gad_som_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_som_adv_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GAD.h
gad_som_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_som_adv_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GAD.h
gad_som_exchanges.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_som_exchanges.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h GAD.h
gad_som_exchanges.f: GAD_SOM_VARS.h
gad_som_fill_cs_corner.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_som_fill_cs_corner.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
gad_som_fill_cs_corner.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GAD.h
gad_som_lim_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_som_lim_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GAD.h
gad_som_prep_cs_corner.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_som_prep_cs_corner.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
gad_som_prep_cs_corner.f: GAD.h
gad_u3_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_u3_adv_r.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_u3_adv_r.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_u3_adv_r.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_u3_adv_r.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_u3_adv_r.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_u3_adv_r.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_u3_adv_r.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_u3_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_u3_adv_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_u3_adv_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_u3_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_u3_adv_x.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_u3_adv_x.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_u3_adv_x.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_u3_adv_x.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_u3_adv_x.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_u3_adv_x.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_u3_adv_x.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_u3_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_u3_adv_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_u3_adv_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_u3_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_u3_adv_y.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_u3_adv_y.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_u3_adv_y.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_u3_adv_y.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_u3_adv_y.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_u3_adv_y.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_u3_adv_y.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_u3_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_u3_adv_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_u3_adv_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_u3c4_impl_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_u3c4_impl_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
gad_u3c4_impl_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_u3c4_impl_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_u3c4_impl_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_u3c4_impl_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_u3c4_impl_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_u3c4_impl_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_u3c4_impl_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_u3c4_impl_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_u3c4_impl_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_u3c4_impl_r.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_u3c4_impl_r.f: FCORI_MACROS.h GAD.h
gad_valid_advscheme.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_valid_advscheme.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GAD.h
gad_write_pickup.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_write_pickup.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
gad_write_pickup.f: PARAMS_MACROS.h FCORI_MACROS.h GAD.h GAD_SOM_VARS.h
salt_fill.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
salt_fill.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
salt_fill.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
salt_fill.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
salt_fill.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
salt_fill.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
salt_fill.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
salt_fill.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
salt_fill.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
salt_fill.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
salt_fill.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mdsio_buffertorl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
mdsio_buffertors.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
mdsio_facef_read.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_facef_read.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mdsio_gl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_gl.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mdsio_gl.f: FCORI_MACROS.h
mdsio_gl_slice.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_gl_slice.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mdsio_gl_slice.f: PARAMS_MACROS.h FCORI_MACROS.h
mdsio_pass_r4torl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_pass_r4torl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h
mdsio_pass_r4tors.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_pass_r4tors.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h
mdsio_pass_r8torl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_pass_r8torl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h
mdsio_pass_r8tors.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_pass_r8tors.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h
mdsio_pass_wh_r4torl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_pass_wh_r4torl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h
mdsio_pass_wh_r4tors.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_pass_wh_r4tors.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h
mdsio_pass_wh_r8torl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_pass_wh_r8torl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h
mdsio_pass_wh_r8tors.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_pass_wh_r8tors.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h
mdsio_rd_rec_rl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_rd_rec_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h PARAMS.h
mdsio_rd_rec_rl.f: PARAMS_MACROS.h FCORI_MACROS.h
mdsio_rd_rec_rs.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_rd_rec_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h PARAMS.h
mdsio_rd_rec_rs.f: PARAMS_MACROS.h FCORI_MACROS.h
mdsio_read_field.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_read_field.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mdsio_read_field.f: PARAMS_MACROS.h FCORI_MACROS.h EEBUFF_SCPU.h
mdsio_read_field.f: MDSIO_BUFF_3D.h
mdsio_read_meta.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_read_meta.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mdsio_read_meta.f: PARAMS_MACROS.h FCORI_MACROS.h
mdsio_read_section.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_read_section.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mdsio_read_section.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_readvector.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_readvector.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mdsio_readvector.f: PARAMS_MACROS.h FCORI_MACROS.h EESUPPORT.h mpif.h
mdsio_read_whalos.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_read_whalos.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h
mdsio_read_whalos.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_rw_field.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_rw_field.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mdsio_rw_slice.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_rw_slice.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mdsio_seg4torl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_seg4torl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mdsio_seg4tors.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_seg4tors.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mdsio_seg8torl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_seg8torl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mdsio_seg8tors.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_seg8tors.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mdsio_segxtorx_2d.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_segxtorx_2d.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mdsio_write_field.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_write_field.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mdsio_write_field.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h EEBUFF_SCPU.h
mdsio_write_field.f: MDSIO_BUFF_3D.h
mdsio_writelocal.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_writelocal.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mdsio_writelocal.f: PARAMS_MACROS.h FCORI_MACROS.h MDSIO_BUFF_3D.h
mdsio_write_meta.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_write_meta.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mdsio_write_section.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_write_section.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mdsio_write_section.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_writevec_loc.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_writevec_loc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mdsio_writevec_loc.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_writevector.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_writevector.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mdsio_writevector.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h EESUPPORT.h
mdsio_writevector.f: mpif.h
mdsio_write_whalos.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_write_whalos.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h
mdsio_write_whalos.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_wr_metafiles.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_wr_metafiles.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mdsio_wr_metafiles.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_wr_rec_rl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_wr_rec_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h PARAMS.h
mdsio_wr_rec_rl.f: PARAMS_MACROS.h FCORI_MACROS.h
mdsio_wr_rec_rs.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_wr_rec_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h SIZE.h PARAMS.h
mdsio_wr_rec_rs.f: PARAMS_MACROS.h FCORI_MACROS.h
mom_calc_absvort3.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_absvort3.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_calc_absvort3.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_calc_absvort3.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_calc_absvort3.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_calc_absvort3.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_calc_absvort3.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_calc_absvort3.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_calc_absvort3.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_calc_absvort3.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_calc_absvort3.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_calc_absvort3.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_calc_absvort3.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_calc_hdiv.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_hdiv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_calc_hdiv.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_calc_hdiv.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_calc_hdiv.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_calc_hdiv.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_hdiv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_hdiv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_hdiv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_calc_hdiv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_calc_hdiv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_calc_hdiv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_calc_hdiv.f: TANPHIATV_MACROS.h
mom_calc_hfacz.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_hfacz.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_calc_hfacz.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_calc_hfacz.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_calc_hfacz.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_calc_hfacz.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_hfacz.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_hfacz.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_hfacz.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_calc_hfacz.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_calc_hfacz.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_calc_hfacz.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_calc_hfacz.f: TANPHIATV_MACROS.h
mom_calc_ke.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_ke.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
mom_calc_ke.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_calc_ke.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_calc_ke.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_ke.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_ke.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_ke.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_calc_ke.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_calc_ke.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_calc_ke.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_calc_ke.f: TANPHIATV_MACROS.h
mom_calc_relvort3.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_relvort3.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_calc_relvort3.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_calc_relvort3.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_calc_relvort3.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_calc_relvort3.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_calc_relvort3.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_calc_relvort3.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_calc_relvort3.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_calc_relvort3.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_calc_relvort3.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_calc_relvort3.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_calc_relvort3.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_calc_strain.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_strain.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_calc_strain.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_calc_strain.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_calc_strain.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_calc_strain.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_strain.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_strain.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_strain.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_calc_strain.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_calc_strain.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_calc_strain.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_calc_strain.f: TANPHIATV_MACROS.h
mom_calc_tension.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_tension.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_calc_tension.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_calc_tension.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_calc_tension.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_calc_tension.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_tension.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_tension.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_tension.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_calc_tension.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_calc_tension.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_calc_tension.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_calc_tension.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_calc_visc.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_visc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
mom_calc_visc.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_calc_visc.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_calc_visc.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_visc.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_visc.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_visc.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_calc_visc.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_calc_visc.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_calc_visc.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_calc_visc.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mom_calc_visc.f: FCORI_MACROS.h NH_VARS.h MOM_VISC.h
mom_diagnostics_init.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_diagnostics_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_diagnostics_init.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_hdissip.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_hdissip.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h GRID.h
mom_hdissip.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_hdissip.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_hdissip.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_hdissip.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_hdissip.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_hdissip.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_hdissip.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_hdissip.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_hdissip.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_hdissip.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_init_fixed.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_init_fixed.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_init_fixed.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_init_fixed.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_init_fixed.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_init_fixed.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_init_fixed.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_init_fixed.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_init_fixed.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_init_fixed.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_init_fixed.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_init_fixed.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_init_fixed.f: TANPHIATV_MACROS.h SURFACE.h MOM_VISC.h
mom_quasihydrostatic.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_quasihydrostatic.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_quasihydrostatic.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_quasihydrostatic.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_quasihydrostatic.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_quasihydrostatic.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_quasihydrostatic.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_quasihydrostatic.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_quasihydrostatic.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_quasihydrostatic.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_quasihydrostatic.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_quasihydrostatic.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mom_quasihydrostatic.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
mom_quasihydrostatic.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_u_bottomdrag.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_bottomdrag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_bottomdrag.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_bottomdrag.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_bottomdrag.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_bottomdrag.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_bottomdrag.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_bottomdrag.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_bottomdrag.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_u_bottomdrag.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_u_bottomdrag.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_u_bottomdrag.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_u_bottomdrag.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_u_coriolis_nh.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_coriolis_nh.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_u_coriolis_nh.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_u_coriolis_nh.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_u_coriolis_nh.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_u_coriolis_nh.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_u_coriolis_nh.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_u_coriolis_nh.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_u_coriolis_nh.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_u_coriolis_nh.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_u_coriolis_nh.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_u_coriolis_nh.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_u_coriolis_nh.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_u_implicit_r.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_implicit_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_implicit_r.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_implicit_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_implicit_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_implicit_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_implicit_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_implicit_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_implicit_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_u_implicit_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_u_implicit_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_u_implicit_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_u_implicit_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
mom_u_metric_nh.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_metric_nh.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_metric_nh.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_metric_nh.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_metric_nh.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_metric_nh.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_metric_nh.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_metric_nh.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_metric_nh.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_metric_nh.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_metric_nh.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_metric_nh.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_metric_nh.f: TANPHIATV_MACROS.h
mom_u_rviscflux.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_rviscflux.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_rviscflux.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_rviscflux.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_rviscflux.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_rviscflux.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_rviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_rviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_rviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_rviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_rviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_rviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_rviscflux.f: TANPHIATV_MACROS.h
mom_u_sidedrag.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_sidedrag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_sidedrag.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_sidedrag.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_sidedrag.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_sidedrag.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_sidedrag.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_sidedrag.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_sidedrag.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_sidedrag.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_sidedrag.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_sidedrag.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_sidedrag.f: TANPHIATV_MACROS.h SURFACE.h
mom_v_bottomdrag.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_bottomdrag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_bottomdrag.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_bottomdrag.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_bottomdrag.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_bottomdrag.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_bottomdrag.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_bottomdrag.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_bottomdrag.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_v_bottomdrag.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_v_bottomdrag.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_v_bottomdrag.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_v_bottomdrag.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_v_coriolis_nh.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_coriolis_nh.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_v_coriolis_nh.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_v_coriolis_nh.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_v_coriolis_nh.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_v_coriolis_nh.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_v_coriolis_nh.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_v_coriolis_nh.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_v_coriolis_nh.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_v_coriolis_nh.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_v_coriolis_nh.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_v_coriolis_nh.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_v_coriolis_nh.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_v_implicit_r.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_implicit_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_implicit_r.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_implicit_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_implicit_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_implicit_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_implicit_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_implicit_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_implicit_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_v_implicit_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_v_implicit_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_v_implicit_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_v_implicit_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
mom_v_metric_nh.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_metric_nh.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_metric_nh.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_metric_nh.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_metric_nh.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_metric_nh.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_metric_nh.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_metric_nh.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_metric_nh.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_metric_nh.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_metric_nh.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_metric_nh.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_metric_nh.f: TANPHIATV_MACROS.h
mom_v_rviscflux.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_rviscflux.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_rviscflux.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_rviscflux.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_rviscflux.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_rviscflux.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_rviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_rviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_rviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_rviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_rviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_rviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_rviscflux.f: TANPHIATV_MACROS.h
mom_v_sidedrag.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_sidedrag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_sidedrag.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_sidedrag.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_sidedrag.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_sidedrag.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_sidedrag.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_sidedrag.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_sidedrag.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_sidedrag.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_sidedrag.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_sidedrag.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_sidedrag.f: TANPHIATV_MACROS.h SURFACE.h
mom_w_coriolis_nh.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_w_coriolis_nh.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_w_coriolis_nh.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_w_coriolis_nh.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_w_coriolis_nh.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_w_coriolis_nh.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_w_coriolis_nh.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_w_coriolis_nh.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_w_coriolis_nh.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_w_coriolis_nh.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_w_coriolis_nh.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_w_coriolis_nh.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_w_coriolis_nh.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_w_metric_nh.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_w_metric_nh.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_w_metric_nh.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_w_metric_nh.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_w_metric_nh.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_w_metric_nh.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_w_metric_nh.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_w_metric_nh.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_w_metric_nh.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_w_metric_nh.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_w_metric_nh.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_w_metric_nh.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_w_metric_nh.f: TANPHIATV_MACROS.h
mom_w_sidedrag.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_w_sidedrag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_w_sidedrag.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_w_sidedrag.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_w_sidedrag.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_w_sidedrag.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_w_sidedrag.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_w_sidedrag.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_w_sidedrag.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_w_sidedrag.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_w_sidedrag.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_w_sidedrag.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_w_sidedrag.f: TANPHIATV_MACROS.h SURFACE.h
mom_calc_rtrans.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_rtrans.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h EEPARAMS.h
mom_calc_rtrans.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_calc_rtrans.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_calc_rtrans.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_calc_rtrans.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_rtrans.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_rtrans.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_rtrans.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_calc_rtrans.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_calc_rtrans.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_calc_rtrans.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_calc_rtrans.f: TANPHIATV_MACROS.h SURFACE.h MOM_FLUXFORM.h
mom_fluxform.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_fluxform.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h FFIELDS.h
mom_fluxform.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_fluxform.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_fluxform.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_fluxform.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_fluxform.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_fluxform.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_fluxform.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_fluxform.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_fluxform.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_fluxform.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_fluxform.f: TANPHIATV_MACROS.h SURFACE.h
mom_u_adv_uu.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_adv_uu.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_adv_uu.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_adv_uu.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_adv_uu.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_adv_uu.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_adv_uu.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_adv_uu.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_adv_uu.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_adv_uu.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_adv_uu.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_adv_uu.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_adv_uu.f: TANPHIATV_MACROS.h
mom_u_adv_vu.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_adv_vu.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_adv_vu.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_adv_vu.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_adv_vu.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_adv_vu.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_adv_vu.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_adv_vu.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_adv_vu.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_adv_vu.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_adv_vu.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_adv_vu.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_adv_vu.f: TANPHIATV_MACROS.h
mom_u_adv_wu.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_adv_wu.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_adv_wu.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_adv_wu.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_adv_wu.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_adv_wu.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_adv_wu.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_adv_wu.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_adv_wu.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_adv_wu.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_adv_wu.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_adv_wu.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_adv_wu.f: TANPHIATV_MACROS.h
mom_u_coriolis.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_coriolis.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_coriolis.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_coriolis.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_coriolis.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_coriolis.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_coriolis.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_coriolis.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_coriolis.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_coriolis.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_coriolis.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_coriolis.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_coriolis.f: TANPHIATV_MACROS.h SURFACE.h
mom_u_del2u.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_del2u.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_del2u.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_del2u.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_del2u.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_del2u.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_del2u.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_del2u.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_del2u.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_del2u.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_del2u.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_del2u.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_del2u.f: TANPHIATV_MACROS.h
mom_u_metric_cylinder.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_u_metric_cylinder.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_u_metric_cylinder.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_u_metric_cylinder.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_u_metric_cylinder.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_u_metric_cylinder.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_u_metric_cylinder.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_metric_cylinder.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_u_metric_cylinder.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_u_metric_cylinder.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_metric_cylinder.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
mom_u_metric_cylinder.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h
mom_u_metric_cylinder.f: RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_u_metric_cylinder.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_u_metric_sphere.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_metric_sphere.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_u_metric_sphere.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_u_metric_sphere.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_u_metric_sphere.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_u_metric_sphere.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_u_metric_sphere.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_u_metric_sphere.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_u_metric_sphere.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_metric_sphere.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_u_metric_sphere.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_u_metric_sphere.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mom_u_metric_sphere.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
mom_u_metric_sphere.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_uv_boundary.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_uv_boundary.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_uv_boundary.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_uv_boundary.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_uv_boundary.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_uv_boundary.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_uv_boundary.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_uv_boundary.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_uv_boundary.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_uv_boundary.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_uv_boundary.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_uv_boundary.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_uv_boundary.f: TANPHIATV_MACROS.h
mom_u_xviscflux.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_xviscflux.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_xviscflux.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_xviscflux.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_xviscflux.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_xviscflux.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_xviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_xviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_xviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_xviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_xviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_xviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_xviscflux.f: TANPHIATV_MACROS.h
mom_u_yviscflux.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_yviscflux.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_u_yviscflux.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_yviscflux.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_yviscflux.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_yviscflux.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_yviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_yviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_yviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_yviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_yviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_yviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_yviscflux.f: TANPHIATV_MACROS.h
mom_v_adv_uv.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_adv_uv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_adv_uv.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_adv_uv.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_adv_uv.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_adv_uv.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_adv_uv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_adv_uv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_adv_uv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_adv_uv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_adv_uv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_adv_uv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_adv_uv.f: TANPHIATV_MACROS.h
mom_v_adv_vv.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_adv_vv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_adv_vv.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_adv_vv.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_adv_vv.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_adv_vv.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_adv_vv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_adv_vv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_adv_vv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_adv_vv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_adv_vv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_adv_vv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_adv_vv.f: TANPHIATV_MACROS.h
mom_v_adv_wv.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_adv_wv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_adv_wv.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_adv_wv.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_adv_wv.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_adv_wv.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_adv_wv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_adv_wv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_adv_wv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_adv_wv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_adv_wv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_adv_wv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_adv_wv.f: TANPHIATV_MACROS.h
mom_v_coriolis.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_coriolis.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_coriolis.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_coriolis.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_coriolis.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_coriolis.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_coriolis.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_coriolis.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_coriolis.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_coriolis.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_coriolis.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_coriolis.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_coriolis.f: TANPHIATV_MACROS.h SURFACE.h
mom_v_del2v.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_del2v.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_del2v.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_del2v.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_del2v.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_del2v.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_del2v.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_del2v.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_del2v.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_del2v.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_del2v.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_del2v.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_del2v.f: TANPHIATV_MACROS.h
mom_v_metric_cylinder.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_v_metric_cylinder.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_v_metric_cylinder.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_v_metric_cylinder.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_v_metric_cylinder.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_v_metric_cylinder.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_v_metric_cylinder.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_metric_cylinder.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_v_metric_cylinder.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_v_metric_cylinder.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_metric_cylinder.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
mom_v_metric_cylinder.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h
mom_v_metric_cylinder.f: RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_v_metric_cylinder.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_v_metric_sphere.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_metric_sphere.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_v_metric_sphere.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_v_metric_sphere.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_v_metric_sphere.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_v_metric_sphere.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_v_metric_sphere.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_v_metric_sphere.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_v_metric_sphere.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_metric_sphere.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_v_metric_sphere.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_v_metric_sphere.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mom_v_metric_sphere.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
mom_v_metric_sphere.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_v_xviscflux.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_xviscflux.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_xviscflux.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_xviscflux.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_xviscflux.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_xviscflux.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_xviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_xviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_xviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_xviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_xviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_xviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_xviscflux.f: TANPHIATV_MACROS.h
mom_v_yviscflux.f: MOM_FLUXFORM_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_yviscflux.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mom_v_yviscflux.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_yviscflux.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_yviscflux.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_yviscflux.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_yviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_yviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_yviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_yviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_yviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_yviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_yviscflux.f: TANPHIATV_MACROS.h
mom_vecinv.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vecinv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h EEPARAMS.h
mom_vecinv.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_vecinv.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_vecinv.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_vecinv.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_vecinv.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_vecinv.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_vecinv.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vecinv.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_vecinv.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_vecinv.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h TIMEAVE_STATV.h
mom_vecinv.f: TIMEAVE_OPTIONS.h
mom_vi_coriolis.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_coriolis.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h GRID.h
mom_vi_coriolis.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_coriolis.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_coriolis.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vi_coriolis.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_coriolis.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_coriolis.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_coriolis.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_coriolis.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_coriolis.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_coriolis.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_vi_del2uv.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_del2uv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
mom_vi_del2uv.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_vi_del2uv.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_vi_del2uv.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vi_del2uv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_del2uv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_del2uv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_del2uv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_del2uv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_del2uv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_del2uv.f: TANPHIATV_MACROS.h EEPARAMS.h
mom_vi_hdissip.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_hdissip.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
mom_vi_hdissip.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_vi_hdissip.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_vi_hdissip.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vi_hdissip.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_hdissip.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_hdissip.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_hdissip.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_hdissip.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_hdissip.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_hdissip.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mom_vi_hdissip.f: FCORI_MACROS.h
mom_vi_u_coriolis_c4.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_u_coriolis_c4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_vi_u_coriolis_c4.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_vi_u_coriolis_c4.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_u_coriolis_c4.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_u_coriolis_c4.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_u_coriolis_c4.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_u_coriolis_c4.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_vi_u_coriolis_c4.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_u_coriolis_c4.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_u_coriolis_c4.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_u_coriolis_c4.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mom_vi_u_coriolis_c4.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
mom_vi_u_coriolis_c4.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_u_coriolis.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_u_coriolis.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h GRID.h
mom_vi_u_coriolis.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_u_coriolis.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_u_coriolis.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_u_coriolis.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_u_coriolis.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_vi_u_coriolis.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_u_coriolis.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_u_coriolis.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_vi_u_coriolis.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_vi_u_coriolis.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h PARAMS.h
mom_vi_u_coriolis.f: PARAMS_MACROS.h FCORI_MACROS.h
mom_vi_u_grad_ke.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_u_grad_ke.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h GRID.h
mom_vi_u_grad_ke.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_u_grad_ke.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_u_grad_ke.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vi_u_grad_ke.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_u_grad_ke.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_u_grad_ke.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_u_grad_ke.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_u_grad_ke.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_vi_u_grad_ke.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_vi_u_grad_ke.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_u_vertshear.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_u_vertshear.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h GRID.h
mom_vi_u_vertshear.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_u_vertshear.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_u_vertshear.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_u_vertshear.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_u_vertshear.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_vi_u_vertshear.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_u_vertshear.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_u_vertshear.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_u_vertshear.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mom_vi_u_vertshear.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
mom_vi_u_vertshear.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_u_vertshear.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_vi_v_coriolis_c4.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_v_coriolis_c4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_vi_v_coriolis_c4.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_vi_v_coriolis_c4.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_v_coriolis_c4.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_v_coriolis_c4.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_v_coriolis_c4.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_v_coriolis_c4.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_vi_v_coriolis_c4.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_v_coriolis_c4.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_v_coriolis_c4.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_v_coriolis_c4.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mom_vi_v_coriolis_c4.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
mom_vi_v_coriolis_c4.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_v_coriolis.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_v_coriolis.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h GRID.h
mom_vi_v_coriolis.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_v_coriolis.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_v_coriolis.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_v_coriolis.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_v_coriolis.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_vi_v_coriolis.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_v_coriolis.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_v_coriolis.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_vi_v_coriolis.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_vi_v_coriolis.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h PARAMS.h
mom_vi_v_coriolis.f: PARAMS_MACROS.h FCORI_MACROS.h
mom_vi_v_grad_ke.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_v_grad_ke.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h GRID.h
mom_vi_v_grad_ke.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_v_grad_ke.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_v_grad_ke.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vi_v_grad_ke.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_v_grad_ke.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_v_grad_ke.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_v_grad_ke.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_v_grad_ke.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_vi_v_grad_ke.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_vi_v_grad_ke.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_v_vertshear.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_v_vertshear.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h GRID.h
mom_vi_v_vertshear.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_v_vertshear.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_v_vertshear.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_v_vertshear.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_v_vertshear.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_vi_v_vertshear.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_v_vertshear.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_v_vertshear.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_v_vertshear.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mom_vi_v_vertshear.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
mom_vi_v_vertshear.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_v_vertshear.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mon_advcfl.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_advcfl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h MONITOR.h
mon_advcflw2.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_advcflw2.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h MONITOR.h
mon_advcflw.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_advcflw.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h MONITOR.h
mon_calc_stats_rl.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_calc_stats_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_calc_stats_rs.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_calc_stats_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_init.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mon_init.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mon_init.f: FCORI_MACROS.h MONITOR.h
monitor_ad.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
monitor_ad.f: CPP_EEOPTIONS.h CPP_EEMACROS.h AD_CONFIG.h SIZE.h EEPARAMS.h
monitor_ad.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
monitor_ad.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
monitor_ad.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
monitor_ad.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
monitor_ad.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
monitor_ad.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
monitor_ad.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
monitor_ad.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
monitor_ad.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
monitor_ad.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h FFIELDS.h
monitor_ad.f: MONITOR.h
monitor.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
monitor.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
monitor.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
monitor.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
monitor.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
monitor.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
monitor.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
monitor.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
monitor.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
monitor.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
monitor.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
monitor.f: FFIELDS.h MONITOR.h
monitor_g.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
monitor_g.f: CPP_EEOPTIONS.h CPP_EEMACROS.h AD_CONFIG.h SIZE.h EEPARAMS.h
monitor_g.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
monitor_g.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
monitor_g.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
monitor_g.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
monitor_g.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
monitor_g.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
monitor_g.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
monitor_g.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
monitor_g.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
monitor_g.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h FFIELDS.h
monitor_g.f: MONITOR.h
mon_ke.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mon_ke.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mon_ke.f: FCORI_MACROS.h DYNVARS.h MONITOR.h GRID.h GRID_MACROS.h
mon_ke.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
mon_ke.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mon_ke.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mon_ke.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mon_ke.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mon_ke.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
mon_ke.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mon_ke.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mon_ke.f: SURFACE.h
mon_out.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mon_out.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mon_out.f: FCORI_MACROS.h MONITOR.h
mon_printstats_rl.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_printstats_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_printstats_rl.f: MONITOR.h
mon_printstats_rs.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_printstats_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_printstats_rs.f: MONITOR.h
mon_set_iounit.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_set_iounit.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h MONITOR.h
mon_set_pref.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_set_pref.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h MONITOR.h
mon_solution.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_solution.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mon_solution.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h GRID_MACROS.h
mon_solution.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mon_solution.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mon_solution.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mon_solution.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mon_solution.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mon_solution.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mon_solution.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mon_solution.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mon_solution.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mon_solution.f: TANPHIATV_MACROS.h MONITOR.h
mon_stats_latbnd_rl.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_stats_latbnd_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_stats_rl.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_stats_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_stats_rs.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_stats_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_surfcor.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_surfcor.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mon_surfcor.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h SURFACE.h GRID.h
mon_surfcor.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mon_surfcor.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mon_surfcor.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mon_surfcor.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mon_surfcor.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mon_surfcor.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mon_surfcor.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mon_surfcor.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mon_surfcor.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mon_surfcor.f: TANPHIATV_MACROS.h MONITOR.h
mon_vort3.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_vort3.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mon_vort3.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h MONITOR.h GRID.h
mon_vort3.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mon_vort3.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mon_vort3.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mon_vort3.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mon_vort3.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mon_vort3.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mon_vort3.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mon_vort3.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mon_vort3.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mon_writestats_rl.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_writestats_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_writestats_rl.f: MONITOR.h
mon_writestats_rs.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_writestats_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_writestats_rs.f: MONITOR.h
obcs_apply_eta.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_apply_eta.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_apply_eta.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
obcs_apply_eta.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
obcs_apply_eta.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
obcs_apply_eta.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_apply_eta.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_apply_eta.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_eta.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_apply_eta.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_apply_eta.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_apply_eta.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_apply_eta.f: TANPHIATV_MACROS.h SURFACE.h OBCS.h
obcs_apply_ptracer.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_apply_ptracer.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
obcs_apply_ptracer.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
obcs_apply_ptracer.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
obcs_apply_ptracer.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
obcs_apply_ptracer.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
obcs_apply_ptracer.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
obcs_apply_ptracer.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
obcs_apply_ptracer.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_ptracer.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
obcs_apply_ptracer.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
obcs_apply_ptracer.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
obcs_apply_ptracer.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
obcs_apply_ptracer.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
obcs_apply_ptracer.f: OBCS.h
obcs_apply_r_star.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_apply_r_star.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
obcs_apply_r_star.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
obcs_apply_r_star.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
obcs_apply_r_star.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
obcs_apply_r_star.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
obcs_apply_r_star.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
obcs_apply_r_star.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
obcs_apply_r_star.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
obcs_apply_r_star.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
obcs_apply_r_star.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
obcs_apply_r_star.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
obcs_apply_r_star.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h OBCS.h
obcs_apply_seaice.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_apply_seaice.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
obcs_apply_seaice.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
obcs_apply_seaice.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
obcs_apply_seaice.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
obcs_apply_seaice.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
obcs_apply_seaice.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
obcs_apply_seaice.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
obcs_apply_seaice.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
obcs_apply_seaice.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
obcs_apply_seaice.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
obcs_apply_seaice.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
obcs_apply_seaice.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h OBCS.h
obcs_apply_sloc.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_apply_sloc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_apply_sloc.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
obcs_apply_sloc.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
obcs_apply_sloc.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
obcs_apply_sloc.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_apply_sloc.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_apply_sloc.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_sloc.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_apply_sloc.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_apply_sloc.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_apply_sloc.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_apply_sloc.f: TANPHIATV_MACROS.h OBCS.h
obcs_apply_surf_dr.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_apply_surf_dr.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
obcs_apply_surf_dr.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
obcs_apply_surf_dr.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
obcs_apply_surf_dr.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
obcs_apply_surf_dr.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
obcs_apply_surf_dr.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
obcs_apply_surf_dr.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
obcs_apply_surf_dr.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_surf_dr.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
obcs_apply_surf_dr.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
obcs_apply_surf_dr.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
obcs_apply_surf_dr.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
obcs_apply_surf_dr.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
obcs_apply_surf_dr.f: OBCS.h
obcs_apply_tloc.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_apply_tloc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_apply_tloc.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
obcs_apply_tloc.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
obcs_apply_tloc.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
obcs_apply_tloc.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_apply_tloc.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_apply_tloc.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_tloc.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_apply_tloc.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_apply_tloc.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_apply_tloc.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_apply_tloc.f: TANPHIATV_MACROS.h OBCS.h
obcs_apply_ts.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_apply_ts.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_apply_ts.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
obcs_apply_ts.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
obcs_apply_ts.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
obcs_apply_ts.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_apply_ts.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_apply_ts.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_ts.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_apply_ts.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_apply_ts.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_apply_ts.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_apply_ts.f: TANPHIATV_MACROS.h OBCS.h
obcs_apply_uv.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_apply_uv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_apply_uv.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
obcs_apply_uv.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
obcs_apply_uv.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
obcs_apply_uv.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_apply_uv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_apply_uv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_uv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_apply_uv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_apply_uv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_apply_uv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_apply_uv.f: TANPHIATV_MACROS.h OBCS.h
obcs_apply_uvice.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_apply_uvice.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_apply_uvice.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
obcs_apply_uvice.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
obcs_apply_uvice.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
obcs_apply_uvice.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_apply_uvice.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_apply_uvice.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_uvice.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
obcs_apply_uvice.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
obcs_apply_uvice.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
obcs_apply_uvice.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
obcs_apply_uvice.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h OBCS.h
obcs_apply_w.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_apply_w.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_apply_w.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
obcs_apply_w.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
obcs_apply_w.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
obcs_apply_w.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_apply_w.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_apply_w.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_w.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_apply_w.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_apply_w.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_apply_w.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_apply_w.f: TANPHIATV_MACROS.h OBCS.h
obcs_calc.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
obcs_calc.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
obcs_calc.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
obcs_calc.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
obcs_calc.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_calc.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_calc.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_calc.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_calc.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_calc.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
obcs_calc.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h OBCS.h
obcs_calc_stevens.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_calc_stevens.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
obcs_calc_stevens.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
obcs_calc_stevens.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
obcs_calc_stevens.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
obcs_calc_stevens.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
obcs_calc_stevens.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
obcs_calc_stevens.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
obcs_calc_stevens.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
obcs_calc_stevens.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
obcs_calc_stevens.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
obcs_calc_stevens.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
obcs_calc_stevens.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h OBCS.h DYNVARS.h
obcs_check_depths.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_check_depths.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
obcs_check_depths.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
obcs_check_depths.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
obcs_check_depths.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
obcs_check_depths.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
obcs_check_depths.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
obcs_check_depths.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
obcs_check_depths.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
obcs_check_depths.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
obcs_check_depths.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
obcs_check_depths.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
obcs_check_depths.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h OBCS.h
obcs_check.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
obcs_check.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
obcs_check.f: FCORI_MACROS.h OBCS.h
obcs_copy_tracer.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_copy_tracer.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_copy_tracer.f: PARAMS_MACROS.h FCORI_MACROS.h OBCS.h
obcs_external_fields_load.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_external_fields_load.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
obcs_external_fields_load.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
obcs_external_fields_load.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
obcs_external_fields_load.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
obcs_external_fields_load.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
obcs_external_fields_load.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_external_fields_load.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
obcs_external_fields_load.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
obcs_external_fields_load.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_external_fields_load.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
obcs_external_fields_load.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
obcs_external_fields_load.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
obcs_external_fields_load.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_external_fields_load.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_external_fields_load.f: TANPHIATV_MACROS.h OBCS.h
obcs_init_fixed.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_init_fixed.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_init_fixed.f: PARAMS_MACROS.h FCORI_MACROS.h OBCS.h GRID.h GRID_MACROS.h
obcs_init_fixed.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
obcs_init_fixed.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
obcs_init_fixed.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_init_fixed.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_init_fixed.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_init_fixed.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_init_fixed.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_init_fixed.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_init_fixed.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_init_fixed.f: TANPHIATV_MACROS.h SURFACE.h
obcs_init_variables.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_init_variables.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
obcs_init_variables.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
obcs_init_variables.f: OBCS.h
obcs_output.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
obcs_output.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
obcs_output.f: FCORI_MACROS.h OBCS.h
obcs_prescribe_exchanges.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_prescribe_exchanges.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
obcs_prescribe_exchanges.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
obcs_prescribe_exchanges.f: OBCS.h
obcs_prescribe_read.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_prescribe_read.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
obcs_prescribe_read.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h OBCS.h
obcs_readparms.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_readparms.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_readparms.f: PARAMS_MACROS.h FCORI_MACROS.h OBCS.h ORLANSKI.h
obcs_read_pickup.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_read_pickup.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_read_pickup.f: PARAMS_MACROS.h FCORI_MACROS.h OBCS.h ORLANSKI.h
obcs_sponge.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
obcs_sponge.f: CPP_EEMACROS.h AD_CONFIG.h SIZE.h EEPARAMS.h PARAMS.h
obcs_sponge.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
obcs_sponge.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
obcs_sponge.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
obcs_sponge.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_sponge.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_sponge.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_sponge.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_sponge.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_sponge.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_sponge.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_sponge.f: TANPHIATV_MACROS.h DYNVARS.h OBCS.h
obcs_write_pickup.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
obcs_write_pickup.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
obcs_write_pickup.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h RESTART.h OBCS.h
obcs_write_pickup.f: ORLANSKI.h
orlanski_east.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
orlanski_east.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
orlanski_east.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
orlanski_east.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
orlanski_east.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
orlanski_east.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
orlanski_east.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
orlanski_east.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
orlanski_east.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
orlanski_east.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
orlanski_east.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
orlanski_east.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
orlanski_east.f: TANPHIATV_MACROS.h OBCS.h ORLANSKI.h
orlanski_init.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
orlanski_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
orlanski_init.f: PARAMS_MACROS.h FCORI_MACROS.h ORLANSKI.h
orlanski_north.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
orlanski_north.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
orlanski_north.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
orlanski_north.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
orlanski_north.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
orlanski_north.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
orlanski_north.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
orlanski_north.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
orlanski_north.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
orlanski_north.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
orlanski_north.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
orlanski_north.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
orlanski_north.f: TANPHIATV_MACROS.h OBCS.h ORLANSKI.h
orlanski_south.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
orlanski_south.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
orlanski_south.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
orlanski_south.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
orlanski_south.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
orlanski_south.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
orlanski_south.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
orlanski_south.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
orlanski_south.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
orlanski_south.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
orlanski_south.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
orlanski_south.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
orlanski_south.f: TANPHIATV_MACROS.h OBCS.h ORLANSKI.h
orlanski_west.f: OBCS_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
orlanski_west.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
orlanski_west.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
orlanski_west.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
orlanski_west.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
orlanski_west.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
orlanski_west.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
orlanski_west.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
orlanski_west.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
orlanski_west.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
orlanski_west.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
orlanski_west.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
orlanski_west.f: TANPHIATV_MACROS.h OBCS.h ORLANSKI.h
pp81_calc_diff.f: PP81_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
pp81_calc_diff.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
pp81_calc_diff.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
pp81_calc_diff.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
pp81_calc_diff.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
pp81_calc_diff.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
pp81_calc_diff.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
pp81_calc_diff.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
pp81_calc_diff.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
pp81_calc_diff.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
pp81_calc_diff.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
pp81_calc_diff.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
pp81_calc_diff.f: TANPHIATV_MACROS.h PP81.h
pp81_calc_visc.f: PP81_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
pp81_calc_visc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
pp81_calc_visc.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
pp81_calc_visc.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
pp81_calc_visc.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
pp81_calc_visc.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
pp81_calc_visc.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
pp81_calc_visc.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
pp81_calc_visc.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
pp81_calc_visc.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
pp81_calc_visc.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
pp81_calc_visc.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
pp81_calc_visc.f: TANPHIATV_MACROS.h PP81.h
pp81_check.f: PP81_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
pp81_check.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
pp81_check.f: FCORI_MACROS.h
pp81_init_varia.f: PP81_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
pp81_init_varia.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
pp81_init_varia.f: PARAMS_MACROS.h FCORI_MACROS.h PP81.h
pp81_readparms.f: PP81_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
pp81_readparms.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PP81.h
pp81_readparms.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
pp81_ri_number.f: PP81_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
pp81_ri_number.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
pp81_ri_number.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h PP81.h GRID.h
pp81_ri_number.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
pp81_ri_number.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
pp81_ri_number.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
pp81_ri_number.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
pp81_ri_number.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
pp81_ri_number.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
pp81_ri_number.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
pp81_ri_number.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
pp81_ri_number.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
pp81_ri_number.f: TANPHIATV_MACROS.h
get_write_global_fld.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
read_fld_xy_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
read_fld_xy_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
read_fld_xy_rl.f: PARAMS_MACROS.h FCORI_MACROS.h
read_fld_xy_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
read_fld_xy_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
read_fld_xy_rs.f: PARAMS_MACROS.h FCORI_MACROS.h
read_fld_xyz_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
read_fld_xyz_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
read_fld_xyz_rl.f: PARAMS_MACROS.h FCORI_MACROS.h
read_fld_xyz_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
read_fld_xyz_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
read_fld_xyz_rs.f: PARAMS_MACROS.h FCORI_MACROS.h
read_mflds.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
read_mflds.f: CPP_EEMACROS.h EEPARAMS.h RW_MFLDS.h SIZE.h PARAMS.h
read_mflds.f: PARAMS_MACROS.h FCORI_MACROS.h
read_rec.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
read_rec.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
read_rec.f: FCORI_MACROS.h
set_write_global_fld.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
write_fld_3d_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_3d_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_fld_3d_rl.f: PARAMS_MACROS.h FCORI_MACROS.h
write_fld_3d_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_3d_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_fld_3d_rs.f: PARAMS_MACROS.h FCORI_MACROS.h
write_fld_s3d_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_s3d_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_fld_s3d_rl.f: PARAMS_MACROS.h FCORI_MACROS.h
write_fld_s3d_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_s3d_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_fld_s3d_rs.f: PARAMS_MACROS.h FCORI_MACROS.h
write_fld_xy_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_xy_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_fld_xy_rl.f: PARAMS_MACROS.h FCORI_MACROS.h
write_fld_xy_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_xy_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_fld_xy_rs.f: PARAMS_MACROS.h FCORI_MACROS.h
write_fld_xyz_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_xyz_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_fld_xyz_rl.f: PARAMS_MACROS.h FCORI_MACROS.h
write_fld_xyz_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_xyz_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_fld_xyz_rs.f: PARAMS_MACROS.h FCORI_MACROS.h
write_glvec_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_glvec_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_glvec_rl.f: PARAMS_MACROS.h FCORI_MACROS.h
write_glvec_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_glvec_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_glvec_rs.f: PARAMS_MACROS.h FCORI_MACROS.h
write_local_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_local_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_local_rl.f: PARAMS_MACROS.h FCORI_MACROS.h
write_local_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_local_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
write_local_rs.f: PARAMS_MACROS.h FCORI_MACROS.h
write_rec.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
write_rec.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
write_rec.f: FCORI_MACROS.h
timeave_cumul_1k1t.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_cumul_1t.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_cumul_1vfc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_cumul_2v.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_cumul_2v.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h GRID.h
timeave_cumul_2v.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
timeave_cumul_2v.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
timeave_cumul_2v.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
timeave_cumul_2v.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
timeave_cumul_2v.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
timeave_cumul_2v.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
timeave_cumul_2v.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
timeave_cumul_2v.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
timeave_cumul_2v.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
timeave_cumul_2v.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
timeave_cumul_2vfc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_cumulate.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_cumul_dif_1t.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_cumul_dif_1t.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_cumul_dif_1t.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
timeave_cumul_dif_1t.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
timeave_cumul_dif_1t.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
timeave_cumul_dif_1t.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
timeave_cumul_dif_1t.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
timeave_cumul_dif_1t.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
timeave_cumul_dif_1t.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
timeave_cumul_dif_1t.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
timeave_cumul_dif_1t.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
timeave_cumul_dif_1t.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
timeave_cumul_dif_1t.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
timeave_cumul_dif_1t.f: TANPHIATV_MACROS.h
timeave_init_fixed.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_init_fixed.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_init_fixed.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
timeave_norma_2v.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_normalize.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_normaliz.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_reset.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_reset.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_statvars.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_statvars.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
timeave_statvars.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
timeave_statvars.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
timeave_statvars.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
timeave_statvars.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
timeave_statvars.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
timeave_statvars.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
timeave_statvars.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
timeave_statvars.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
timeave_statvars.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
timeave_statvars.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
timeave_statvars.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h TIMEAVE_STATV.h
timeave_statv_write.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_statv_write.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_statv_write.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
timeave_statv_write.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
timeave_statv_write.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
timeave_statv_write.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
timeave_statv_write.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
timeave_statv_write.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
timeave_statv_write.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
timeave_statv_write.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
timeave_statv_write.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
timeave_statv_write.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
timeave_statv_write.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
timeave_statv_write.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
timeave_statv_write.f: TANPHIATV_MACROS.h TIMEAVE_STATV.h
timeave_surf_flux.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_surf_flux.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timeave_surf_flux.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
timeave_surf_flux.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
timeave_surf_flux.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
timeave_surf_flux.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
timeave_surf_flux.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
timeave_surf_flux.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
timeave_surf_flux.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
timeave_surf_flux.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
timeave_surf_flux.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
timeave_surf_flux.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
timeave_surf_flux.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
timeave_surf_flux.f: SURFACE.h FFIELDS.h TIMEAVE_STATV.h
all_proc_die.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
all_proc_die.f: mpif.h
bar2.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
bar2.f: BAR2.h
bar_check.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
barrier.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
barrier.f: mpif.h BARRIER.h
check_threads.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
check_threads.f: mpif.h
comm_stats.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
comm_stats.f: mpif.h EXCH.h BAR2.h
different_multiple.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
diff_phase_multiple.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
eeboot.f: CPP_EEOPTIONS.h CPP_EEMACROS.h PACKAGES_CONFIG.h SIZE.h EEPARAMS.h
eeboot.f: EESUPPORT.h mpif.h
eeboot_minimal.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
eeboot_minimal.f: EEPARAMS.h EESUPPORT.h mpif.h
eedata_example.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
eedie.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
eeintro_msg.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
eeintro_msg.f: EESUPPORT.h mpif.h
eeset_parms.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
eeset_parms.f: mpif.h
eewrite_eeenv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
eewrite_eeenv.f: mpif.h
exch1_bg_r4_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_bg_r8_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_bg_rl_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_bg_rs_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_r4_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_r4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EXCH.h
exch1_r8_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_r8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EXCH.h
exch1_rl_ad.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EXCH.h
exch1_rl_cube_ad.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_rl_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EXCH.h
exch1_rs_ad.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EXCH.h
exch1_rs_cube_ad.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_rs_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EXCH.h
exch1_uv_r4_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_uv_r8_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_uv_rl_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_uv_rs_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_z_r4_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_z_r8_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_z_rl_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch1_z_rs_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_3d_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_3d_r4.f: EEPARAMS.h
exch_3d_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_3d_r8.f: EEPARAMS.h
exch_3d_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_3d_rl.f: EEPARAMS.h
exch_3d_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_3d_rs.f: EEPARAMS.h
exch_cycle_ebl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_cycle_ebl.f: EESUPPORT.h mpif.h EXCH.h
exch_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_init.f: mpif.h EXCH.h
exch_r4_recv_get_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_r4_recv_get_x.f: EESUPPORT.h mpif.h EXCH.h
exch_r4_recv_get_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_r4_recv_get_y.f: EESUPPORT.h mpif.h EXCH.h
exch_r4_send_put_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_r4_send_put_x.f: EESUPPORT.h mpif.h EXCH.h
exch_r4_send_put_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_r4_send_put_y.f: EESUPPORT.h mpif.h EXCH.h
exch_r8_recv_get_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_r8_recv_get_x.f: EESUPPORT.h mpif.h EXCH.h
exch_r8_recv_get_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_r8_recv_get_y.f: EESUPPORT.h mpif.h EXCH.h
exch_r8_send_put_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_r8_send_put_x.f: EESUPPORT.h mpif.h EXCH.h
exch_r8_send_put_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_r8_send_put_y.f: EESUPPORT.h mpif.h EXCH.h
exch_rl_recv_get_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rl_recv_get_x.f: EESUPPORT.h mpif.h EXCH.h
exch_rl_recv_get_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rl_recv_get_y.f: EESUPPORT.h mpif.h EXCH.h
exch_rl_send_put_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rl_send_put_x.f: EESUPPORT.h mpif.h EXCH.h
exch_rl_send_put_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rl_send_put_y.f: EESUPPORT.h mpif.h EXCH.h
exch_rs_recv_get_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rs_recv_get_x.f: EESUPPORT.h mpif.h EXCH.h
exch_rs_recv_get_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rs_recv_get_y.f: EESUPPORT.h mpif.h EXCH.h
exch_rs_send_put_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rs_send_put_x.f: EESUPPORT.h mpif.h EXCH.h
exch_rs_send_put_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rs_send_put_y.f: EESUPPORT.h mpif.h EXCH.h
exch_s3d_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_s3d_r4.f: EEPARAMS.h
exch_s3d_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_s3d_r8.f: EEPARAMS.h
exch_s3d_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_s3d_rl.f: EEPARAMS.h
exch_s3d_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_s3d_rs.f: EEPARAMS.h
exch_sm_3d_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_sm_3d_r4.f: EEPARAMS.h
exch_sm_3d_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_sm_3d_r8.f: EEPARAMS.h
exch_sm_3d_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_sm_3d_rl.f: EEPARAMS.h
exch_sm_3d_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_sm_3d_rs.f: EEPARAMS.h
exch_uv_3d_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_3d_r4.f: EEPARAMS.h
exch_uv_3d_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_3d_r8.f: EEPARAMS.h
exch_uv_3d_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_3d_rl.f: EEPARAMS.h
exch_uv_3d_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_3d_rs.f: EEPARAMS.h
exch_uv_agrid_3d_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_3d_r4.f: SIZE.h EEPARAMS.h
exch_uv_agrid_3d_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_3d_r8.f: SIZE.h EEPARAMS.h
exch_uv_agrid_3d_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_3d_rl.f: SIZE.h EEPARAMS.h
exch_uv_agrid_3d_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_3d_rs.f: SIZE.h EEPARAMS.h
exch_uv_bgrid_3d_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_bgrid_3d_r4.f: SIZE.h EEPARAMS.h
exch_uv_bgrid_3d_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_bgrid_3d_r8.f: SIZE.h EEPARAMS.h
exch_uv_bgrid_3d_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_bgrid_3d_rl.f: SIZE.h EEPARAMS.h
exch_uv_bgrid_3d_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_bgrid_3d_rs.f: SIZE.h EEPARAMS.h
exch_uv_dgrid_3d_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_dgrid_3d_r4.f: SIZE.h EEPARAMS.h
exch_uv_dgrid_3d_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_dgrid_3d_r8.f: SIZE.h EEPARAMS.h
exch_uv_dgrid_3d_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_dgrid_3d_rl.f: SIZE.h EEPARAMS.h
exch_uv_dgrid_3d_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_dgrid_3d_rs.f: SIZE.h EEPARAMS.h
exch_uv_xy_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_xy_r4.f: EEPARAMS.h
exch_uv_xy_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_xy_r8.f: EEPARAMS.h
exch_uv_xy_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_xy_rl.f: EEPARAMS.h
exch_uv_xy_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_xy_rs.f: EEPARAMS.h
exch_uv_xyz_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_xyz_r4.f: EEPARAMS.h
exch_uv_xyz_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_xyz_r8.f: EEPARAMS.h
exch_uv_xyz_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_xyz_rl.f: EEPARAMS.h
exch_uv_xyz_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_uv_xyz_rs.f: EEPARAMS.h
exch_xy_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_xy_r4.f: EEPARAMS.h
exch_xy_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_xy_r8.f: EEPARAMS.h
exch_xy_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_xy_rl.f: EEPARAMS.h
exch_xy_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_xy_rs.f: EEPARAMS.h
exch_xyz_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_xyz_r4.f: EEPARAMS.h
exch_xyz_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_xyz_r8.f: EEPARAMS.h
exch_xyz_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_xyz_rl.f: EEPARAMS.h
exch_xyz_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_xyz_rs.f: EEPARAMS.h
exch_z_3d_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_z_3d_r4.f: EEPARAMS.h
exch_z_3d_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_z_3d_r8.f: EEPARAMS.h
exch_z_3d_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_z_3d_rl.f: EEPARAMS.h
exch_z_3d_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_z_3d_rs.f: EEPARAMS.h
fill_cs_corner_ag_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
fill_cs_corner_ag_rl.f: SIZE.h EEPARAMS.h
fill_cs_corner_tr_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
fill_cs_corner_tr_rl.f: SIZE.h EEPARAMS.h
fill_cs_corner_uv_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
fill_cs_corner_uv_rs.f: SIZE.h EEPARAMS.h
fool_the_compiler.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
gather_2d_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
gather_2d_r4.f: EEPARAMS.h EESUPPORT.h mpif.h
gather_2d_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
gather_2d_r8.f: EEPARAMS.h EESUPPORT.h mpif.h
gather_2d_wh_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
gather_2d_wh_r4.f: EEPARAMS.h EESUPPORT.h mpif.h
gather_2d_wh_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
gather_2d_wh_r8.f: EEPARAMS.h EESUPPORT.h mpif.h
gather_vector.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
gather_vector.f: EEPARAMS.h EESUPPORT.h mpif.h
gather_xz.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
gather_xz.f: EESUPPORT.h mpif.h
gather_yz.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
gather_yz.f: EESUPPORT.h mpif.h
global_max.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
global_max.f: mpif.h GLOBAL_MAX.h
global_sum.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
global_sum.f: mpif.h GLOBAL_SUM.h
global_sum_singlecpu.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
global_sum_singlecpu.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h GLOBAL_SUM.h
global_sum_singlecpu.f: EEBUFF_SCPU.h
global_sum_tile.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
global_sum_tile.f: EESUPPORT.h mpif.h GLOBAL_SUM.h
global_vec_sum.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
global_vec_sum.f: EESUPPORT.h mpif.h GLOBAL_SUM.h
gsum.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
ini_communication_patterns.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h
ini_communication_patterns.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
ini_communication_patterns.f: mpif.h EXCH.h
ini_procs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
ini_procs.f: mpif.h
ini_threading_environment.f: CPP_EEOPTIONS.h CPP_EEMACROS.h PACKAGES_CONFIG.h
ini_threading_environment.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
lef_zero.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
main.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
main.f: EEPARAMS.h EESUPPORT.h mpif.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
main.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
main.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
main.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
main.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
main.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
main.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
main.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h
main.f: RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
main.f: TANPHIATV_MACROS.h DYNVARS.h FFIELDS.h SURFACE.h TIMEAVE_STATV.h
main.f: TIMEAVE_OPTIONS.h SIGREG.h MAIN_PDIRECTIVES1.h MAIN_PDIRECTIVES2.h
master_cpu_io.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
master_cpu_thread.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
master_cpu_thread.f: EESUPPORT.h mpif.h
mds_byteswapi4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
mds_byteswapr4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
mds_byteswapr8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
mds_check4file.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mdsfindunit.f: CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h
mds_reclen.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
memsync.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
nml_filter.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h
nml_set_terminator.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h
open_copy_data_file.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
print.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
scatter_2d_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
scatter_2d_r4.f: EEPARAMS.h EESUPPORT.h mpif.h
scatter_2d_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
scatter_2d_r8.f: EEPARAMS.h EESUPPORT.h mpif.h
scatter_2d_wh_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
scatter_2d_wh_r4.f: EEPARAMS.h EESUPPORT.h mpif.h
scatter_2d_wh_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
scatter_2d_wh_r8.f: EEPARAMS.h EESUPPORT.h mpif.h
scatter_vector.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
scatter_vector.f: EESUPPORT.h mpif.h
scatter_xz.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
scatter_xz.f: mpif.h
scatter_yz.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
scatter_yz.f: mpif.h
stop_if_error.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
timers.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
utils.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
write_utils.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
adams_bashforth2.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
adams_bashforth2.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
adams_bashforth3.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
adams_bashforth3.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_3d_diffusivity.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
calc_3d_diffusivity.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
calc_3d_diffusivity.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
calc_3d_diffusivity.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_3d_diffusivity.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
calc_3d_diffusivity.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
calc_3d_diffusivity.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
calc_3d_diffusivity.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_3d_diffusivity.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_3d_diffusivity.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
calc_3d_diffusivity.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_3d_diffusivity.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
calc_3d_diffusivity.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
calc_3d_diffusivity.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
calc_3d_diffusivity.f: GAD.h
calc_buoyancy.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_buoyancy.f: DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_common_factors.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_common_factors.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_common_factors.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
calc_common_factors.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
calc_common_factors.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
calc_common_factors.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_common_factors.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_common_factors.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
calc_common_factors.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_common_factors.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_common_factors.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_common_factors.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_common_factors.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_common_factors.f: TANPHIATV_MACROS.h
calc_diffusivity.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
calc_diffusivity.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_diffusivity.f: FCORI_MACROS.h DYNVARS.h GRID.h GRID_MACROS.h
calc_diffusivity.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
calc_diffusivity.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
calc_diffusivity.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_diffusivity.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_diffusivity.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_diffusivity.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
calc_diffusivity.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_diffusivity.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_diffusivity.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_diffusivity.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
calc_div_ghat.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_div_ghat.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
calc_div_ghat.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_div_ghat.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
calc_div_ghat.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_div_ghat.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_div_ghat.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_div_ghat.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_div_ghat.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_div_ghat.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_div_ghat.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_div_ghat.f: TANPHIATV_MACROS.h DYNVARS.h
calc_grad_phi_hyd.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_grad_phi_hyd.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_grad_phi_hyd.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
calc_grad_phi_hyd.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
calc_grad_phi_hyd.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
calc_grad_phi_hyd.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
calc_grad_phi_hyd.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
calc_grad_phi_hyd.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
calc_grad_phi_hyd.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_grad_phi_hyd.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_grad_phi_hyd.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_grad_phi_hyd.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
calc_grad_phi_hyd.f: DYNVARS.h
calc_grad_phi_surf.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_grad_phi_surf.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_grad_phi_surf.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
calc_grad_phi_surf.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
calc_grad_phi_surf.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
calc_grad_phi_surf.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
calc_grad_phi_surf.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_grad_phi_surf.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_grad_phi_surf.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
calc_grad_phi_surf.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_grad_phi_surf.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
calc_grad_phi_surf.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
calc_grad_phi_surf.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
calc_grad_phi_surf.f: SURFACE.h
calc_gs.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
calc_gs.f: SIZE.h DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_gs.f: FCORI_MACROS.h RESTART.h GAD.h
calc_gt.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
calc_gt.f: SIZE.h DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_gt.f: FCORI_MACROS.h RESTART.h GAD.h
calc_gw.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
calc_gw.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
calc_gw.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
calc_gw.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
calc_gw.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_gw.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_gw.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_gw.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_gw.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_gw.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_gw.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h RESTART.h SURFACE.h
calc_gw.f: DYNVARS.h NH_VARS.h
calc_ivdc.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
calc_ivdc.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
calc_ivdc.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_ivdc.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
calc_ivdc.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_ivdc.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_ivdc.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_ivdc.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_ivdc.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_ivdc.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_ivdc.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
calc_oce_mxlayer.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
calc_oce_mxlayer.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_oce_mxlayer.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
calc_oce_mxlayer.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
calc_oce_mxlayer.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
calc_oce_mxlayer.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_oce_mxlayer.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_oce_mxlayer.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_oce_mxlayer.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_oce_mxlayer.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_oce_mxlayer.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_oce_mxlayer.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_oce_mxlayer.f: TANPHIATV_MACROS.h DYNVARS.h
calc_phi_hyd.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
calc_phi_hyd.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
calc_phi_hyd.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
calc_phi_hyd.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
calc_phi_hyd.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_phi_hyd.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_phi_hyd.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_phi_hyd.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_phi_hyd.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_phi_hyd.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_phi_hyd.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h
calc_phi_hyd.f: PARAMS_MACROS.h FCORI_MACROS.h SURFACE.h DYNVARS.h
calc_r_star.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
calc_r_star.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_r_star.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_r_star.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
calc_r_star.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_r_star.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_r_star.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_r_star.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_r_star.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_r_star.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_r_star.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_r_star.f: TANPHIATV_MACROS.h SURFACE.h
calc_surf_dr.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_surf_dr.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
calc_surf_dr.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_surf_dr.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
calc_surf_dr.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_surf_dr.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_surf_dr.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_surf_dr.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_surf_dr.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_surf_dr.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_surf_dr.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_surf_dr.f: TANPHIATV_MACROS.h SURFACE.h
calc_viscosity.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
calc_viscosity.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_viscosity.f: FCORI_MACROS.h DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h
calc_viscosity.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
calc_viscosity.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
calc_viscosity.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_viscosity.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_viscosity.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_viscosity.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_viscosity.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_viscosity.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_viscosity.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_viscosity.f: TANPHIATV_MACROS.h
calc_wsurf_tr.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_wsurf_tr.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
calc_wsurf_tr.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_wsurf_tr.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
calc_wsurf_tr.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_wsurf_tr.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_wsurf_tr.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_wsurf_tr.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_wsurf_tr.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_wsurf_tr.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_wsurf_tr.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_wsurf_tr.f: TANPHIATV_MACROS.h SURFACE.h
cg2d.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
cg2d.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h CG2D.h
cg2d_nsa.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
cg2d_nsa.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h CG2D.h
cg2d_sr.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
cg2d_sr.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h CG2D.h
cg3d.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
cg3d.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
cg3d.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
cg3d.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
cg3d.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
cg3d.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
cg3d.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
cg3d.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
cg3d.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
cg3d.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
cg3d.f: SURFACE.h CG3D.h
check_pickup.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
check_pickup.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h RESTART.h
config_check.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
config_check.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
config_check.f: FCORI_MACROS.h
config_summary.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
config_summary.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
config_summary.f: FCORI_MACROS.h EOS.h GRID.h GRID_MACROS.h DXC_MACROS.h
config_summary.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
config_summary.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
config_summary.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
config_summary.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
config_summary.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
config_summary.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
config_summary.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
config_summary.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
config_summary.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
config_summary.f: TANPHIATV_MACROS.h
convective_adjustment.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
convective_adjustment.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
convective_adjustment.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
convective_adjustment.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
convective_adjustment.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
convective_adjustment.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
convective_adjustment.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
convective_adjustment.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
convective_adjustment.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
convective_adjustment.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
convective_adjustment.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
convective_adjustment.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
convective_adjustment.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
convective_adjustment.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
convective_adjustment.f: TIMEAVE_STATV.h TIMEAVE_OPTIONS.h
convective_adjustment_ini.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
convective_adjustment_ini.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
convective_adjustment_ini.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
convective_adjustment_ini.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
convective_adjustment_ini.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
convective_adjustment_ini.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
convective_adjustment_ini.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
convective_adjustment_ini.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
convective_adjustment_ini.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
convective_adjustment_ini.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
convective_adjustment_ini.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
convective_adjustment_ini.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
convective_adjustment_ini.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
convective_adjustment_ini.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
convective_adjustment_ini.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
convective_adjustment_ini.f: TANPHIATV_MACROS.h TIMEAVE_STATV.h
convective_adjustment_ini.f: TIMEAVE_OPTIONS.h
convectively_mixtracer.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
convectively_mixtracer.f: EEPARAMS.h
convective_weights.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
convective_weights.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
convective_weights.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
convective_weights.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
convective_weights.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
convective_weights.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
convective_weights.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
convective_weights.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
convective_weights.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
convective_weights.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
convective_weights.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
convective_weights.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
convective_weights.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
correction_step.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
correction_step.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
correction_step.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
correction_step.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
correction_step.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
correction_step.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
correction_step.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
correction_step.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
correction_step.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
correction_step.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
correction_step.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
correction_step.f: TANPHIATV_MACROS.h DYNVARS.h NH_VARS.h
cycle_tracer.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
cycle_tracer.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diags_oceanic_surf_flux.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
diags_oceanic_surf_flux.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
diags_oceanic_surf_flux.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h
diags_oceanic_surf_flux.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
diags_oceanic_surf_flux.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
diags_oceanic_surf_flux.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
diags_oceanic_surf_flux.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
diags_oceanic_surf_flux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
diags_oceanic_surf_flux.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
diags_oceanic_surf_flux.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
diags_oceanic_surf_flux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
diags_oceanic_surf_flux.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
diags_oceanic_surf_flux.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
diags_oceanic_surf_flux.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
diags_oceanic_surf_flux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
diags_oceanic_surf_flux.f: TANPHIATV_MACROS.h DYNVARS.h SURFACE.h FFIELDS.h
diags_phi_hyd.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
diags_phi_hyd.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
diags_phi_hyd.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
diags_phi_hyd.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
diags_phi_hyd.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
diags_phi_hyd.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
diags_phi_hyd.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
diags_phi_hyd.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
diags_phi_hyd.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
diags_phi_hyd.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
diags_phi_hyd.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
diags_phi_hyd.f: TANPHIATV_MACROS.h SURFACE.h DYNVARS.h
diags_phi_rlow.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
diags_phi_rlow.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
diags_phi_rlow.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
diags_phi_rlow.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
diags_phi_rlow.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
diags_phi_rlow.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
diags_phi_rlow.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
diags_phi_rlow.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
diags_phi_rlow.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
diags_phi_rlow.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
diags_phi_rlow.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
diags_phi_rlow.f: TANPHIATV_MACROS.h SURFACE.h DYNVARS.h
diags_rho.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
diags_rho.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
diags_rho.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
diags_rho.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
diags_rho.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
diags_rho.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
diags_rho.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
diags_rho.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
diags_rho.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
diags_rho.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
diags_rho.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
do_atmospheric_phys.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_atmospheric_phys.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
do_atmospheric_phys.f: PARAMS_MACROS.h FCORI_MACROS.h
do_fields_blocking_exchanges.f: PACKAGES_CONFIG.h CPP_OPTIONS.h
do_fields_blocking_exchanges.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
do_fields_blocking_exchanges.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h
do_fields_blocking_exchanges.f: FCORI_MACROS.h DYNVARS.h GAD.h
do_gterm_blocking_exchanges.f: PACKAGES_CONFIG.h CPP_OPTIONS.h
do_gterm_blocking_exchanges.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
do_gterm_blocking_exchanges.f: EEPARAMS.h DYNVARS.h
do_oceanic_phys.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_oceanic_phys.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
do_oceanic_phys.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
do_oceanic_phys.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
do_oceanic_phys.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
do_oceanic_phys.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
do_oceanic_phys.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
do_oceanic_phys.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
do_oceanic_phys.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
do_oceanic_phys.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
do_oceanic_phys.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
do_oceanic_phys.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
do_oceanic_phys.f: TANPHIATV_MACROS.h DYNVARS.h TIMEAVE_STATV.h
do_oceanic_phys.f: TIMEAVE_OPTIONS.h
do_stagger_fields_exchanges.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
do_stagger_fields_exchanges.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
do_stagger_fields_exchanges.f: FCORI_MACROS.h DYNVARS.h
do_statevars_diags.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_statevars_diags.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
do_statevars_diags.f: PARAMS_MACROS.h FCORI_MACROS.h
do_statevars_tave.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_statevars_tave.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
do_statevars_tave.f: PARAMS_MACROS.h FCORI_MACROS.h
do_the_model_io.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_the_model_io.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
do_the_model_io.f: FCORI_MACROS.h DYNVARS.h
do_write_pickup.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_write_pickup.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
do_write_pickup.f: FCORI_MACROS.h RESTART.h
dynamics.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
dynamics.f: OBCS_OPTIONS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
dynamics.f: FCORI_MACROS.h DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h
dynamics.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
dynamics.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
dynamics.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
dynamics.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
dynamics.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
dynamics.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
dynamics.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
dynamics.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
dynamics.f: TANPHIATV_MACROS.h
external_fields_load.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
external_fields_load.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
external_fields_load.f: PARAMS_MACROS.h FCORI_MACROS.h FFIELDS.h GRID.h
external_fields_load.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
external_fields_load.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
external_fields_load.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
external_fields_load.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
external_fields_load.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
external_fields_load.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
external_fields_load.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
external_fields_load.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
external_fields_load.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
external_fields_load.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
external_fields_load.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
external_fields_load.f: DYNVARS.h
external_forcing_surf.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
external_forcing_surf.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
external_forcing_surf.f: PARAMS_MACROS.h FCORI_MACROS.h FFIELDS.h DYNVARS.h
external_forcing_surf.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
external_forcing_surf.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
external_forcing_surf.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
external_forcing_surf.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
external_forcing_surf.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
external_forcing_surf.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
external_forcing_surf.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
external_forcing_surf.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
external_forcing_surf.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
external_forcing_surf.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h
external_forcing_surf.f: RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
external_forcing_surf.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
find_alpha.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
find_alpha.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h EOS.h GRID.h
find_alpha.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
find_alpha.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
find_alpha.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
find_alpha.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
find_alpha.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
find_alpha.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
find_alpha.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
find_alpha.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
find_alpha.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
find_alpha.f: TANPHIATV_MACROS.h
find_rho.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
find_rho.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h EOS.h
forward_step.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
forward_step.f: CPP_EEMACROS.h OBCS_OPTIONS.h SIZE.h EEPARAMS.h PARAMS.h
forward_step.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h SIGREG.h
freesurf_rescale_g.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
freesurf_rescale_g.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
freesurf_rescale_g.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
freesurf_rescale_g.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
freesurf_rescale_g.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
freesurf_rescale_g.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
freesurf_rescale_g.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
freesurf_rescale_g.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
freesurf_rescale_g.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
freesurf_rescale_g.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
freesurf_rescale_g.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
freesurf_rescale_g.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
freesurf_rescale_g.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
freesurf_rescale_g.f: SURFACE.h
freeze.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
freeze.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
freeze_surface.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
freeze_surface.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
freeze_surface.f: FCORI_MACROS.h DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h
freeze_surface.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
freeze_surface.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
freeze_surface.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
freeze_surface.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
freeze_surface.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
freeze_surface.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
freeze_surface.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
freeze_surface.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
freeze_surface.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
freeze_surface.f: TANPHIATV_MACROS.h FFIELDS.h
grad_sigma.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
grad_sigma.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
grad_sigma.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
grad_sigma.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
grad_sigma.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
grad_sigma.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
grad_sigma.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
grad_sigma.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
grad_sigma.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
grad_sigma.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
grad_sigma.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
grad_sigma.f: FCORI_MACROS.h
impldiff.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
impldiff.f: SIZE.h DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
impldiff.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
impldiff.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
impldiff.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
impldiff.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
impldiff.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
impldiff.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
impldiff.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
impldiff.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
impldiff.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
ini_cartesian_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_cartesian_grid.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_cartesian_grid.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_cartesian_grid.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_cartesian_grid.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_cartesian_grid.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_cartesian_grid.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_cartesian_grid.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_cartesian_grid.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_cartesian_grid.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_cartesian_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
ini_cartesian_grid.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
ini_cartesian_grid.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_cg2d.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_cg2d.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_cg2d.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_cg2d.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_cg2d.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_cg2d.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_cg2d.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_cg2d.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_cg2d.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_cg2d.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_cg2d.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h CG2D.h OBCS.h
ini_cg3d.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_cg3d.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_cg3d.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_cg3d.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_cg3d.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_cg3d.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_cg3d.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_cg3d.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_cg3d.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_cg3d.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_cg3d.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h CG3D.h
ini_cg3d.f: SOLVE_FOR_PRESSURE3D.h OBCS.h
ini_cori.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_cori.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_cori.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_cori.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_cori.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_cori.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_cori.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_cori.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_cori.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_cori.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_cori.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h MONITOR.h
ini_curvilinear_grid.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
ini_curvilinear_grid.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
ini_curvilinear_grid.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_curvilinear_grid.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_curvilinear_grid.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_curvilinear_grid.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_curvilinear_grid.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_curvilinear_grid.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_curvilinear_grid.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_curvilinear_grid.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_curvilinear_grid.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_curvilinear_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
ini_curvilinear_grid.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
ini_curvilinear_grid.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_cylinder_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_cylinder_grid.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_cylinder_grid.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_cylinder_grid.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_cylinder_grid.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_cylinder_grid.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_cylinder_grid.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_cylinder_grid.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_cylinder_grid.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_cylinder_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_cylinder_grid.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_cylinder_grid.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_depths.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_depths.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_depths.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_depths.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_depths.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_depths.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_depths.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_depths.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_depths.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_depths.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_depths.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_depths.f: TANPHIATV_MACROS.h SURFACE.h
ini_dynvars.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_dynvars.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
ini_eos.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_eos.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h EOS.h GRID.h GRID_MACROS.h
ini_eos.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_eos.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_eos.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_eos.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_eos.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_eos.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_eos.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_eos.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_eos.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_fields.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_fields.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_forcing.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_forcing.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_forcing.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_forcing.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_forcing.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_forcing.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_forcing.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_forcing.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_forcing.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_forcing.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_forcing.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_forcing.f: TANPHIATV_MACROS.h SURFACE.h FFIELDS.h
ini_grid.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_grid.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_grid.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_grid.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_grid.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_grid.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_grid.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_grid.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_grid.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_grid.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h MONITOR.h
ini_hfac.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_hfac.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_hfac.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_hfac.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_hfac.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_hfac.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_hfac.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_hfac.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_hfac.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_hfac.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_linear_phisurf.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
ini_linear_phisurf.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
ini_linear_phisurf.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_linear_phisurf.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_linear_phisurf.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_linear_phisurf.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_linear_phisurf.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_linear_phisurf.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_linear_phisurf.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_linear_phisurf.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_linear_phisurf.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_linear_phisurf.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
ini_linear_phisurf.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
ini_linear_phisurf.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_linear_phisurf.f: SURFACE.h
ini_masks_etc.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
ini_masks_etc.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_masks_etc.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
ini_masks_etc.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_masks_etc.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_masks_etc.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_masks_etc.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_masks_etc.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_masks_etc.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_masks_etc.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_masks_etc.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_masks_etc.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
ini_mixing.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_mixing.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_mixing.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_mixing.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_mixing.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_mixing.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_mixing.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_mixing.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_mixing.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_mixing.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_mixing.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_mixing.f: TANPHIATV_MACROS.h DYNVARS.h
ini_mnc_vars.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
ini_mnc_vars.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
ini_mnc_vars.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_mnc_vars.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_mnc_vars.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_mnc_vars.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_mnc_vars.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_mnc_vars.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_mnc_vars.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_mnc_vars.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_mnc_vars.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h
ini_mnc_vars.f: PARAMS_MACROS.h FCORI_MACROS.h
ini_model_io.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
ini_model_io.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h PARAMS.h
ini_model_io.f: PARAMS_MACROS.h FCORI_MACROS.h RESTART.h EEBUFF_SCPU.h
ini_nh_fields.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_nh_fields.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_nh_fields.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_nh_fields.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_nh_fields.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_nh_fields.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_nh_fields.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_nh_fields.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_nh_fields.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_nh_fields.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_nh_fields.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_nh_fields.f: TANPHIATV_MACROS.h RESTART.h NH_VARS.h
ini_nh_vars.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_nh_vars.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h NH_VARS.h
ini_nlfs_vars.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_nlfs_vars.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_nlfs_vars.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_nlfs_vars.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_nlfs_vars.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_nlfs_vars.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_nlfs_vars.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_nlfs_vars.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_nlfs_vars.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_nlfs_vars.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_nlfs_vars.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_nlfs_vars.f: TANPHIATV_MACROS.h SURFACE.h DYNVARS.h
ini_parms.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_parms.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h EOS.h
ini_p_ground.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
ini_p_ground.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_p_ground.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_p_ground.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_p_ground.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_p_ground.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_p_ground.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_p_ground.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_p_ground.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_p_ground.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_p_ground.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_p_ground.f: FCORI_MACROS.h SURFACE.h
ini_pressure.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_pressure.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h EOS.h
ini_pressure.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_pressure.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_pressure.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_pressure.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_pressure.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_pressure.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_pressure.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_pressure.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_pressure.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_pressure.f: TANPHIATV_MACROS.h DYNVARS.h
ini_psurf.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_psurf.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_psurf.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_psurf.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_psurf.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_psurf.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_psurf.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_psurf.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_psurf.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_psurf.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_psurf.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h SURFACE.h
ini_salt.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_salt.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_salt.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_salt.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_salt.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_salt.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_salt.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_salt.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_salt.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_salt.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_salt.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_sigma_hfac.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_sigma_hfac.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_sigma_hfac.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_sigma_hfac.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_sigma_hfac.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_sigma_hfac.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_sigma_hfac.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_sigma_hfac.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_sigma_hfac.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_sigma_hfac.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_sigma_hfac.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_sigma_hfac.f: TANPHIATV_MACROS.h SURFACE.h
ini_spherical_polar_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_spherical_polar_grid.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_spherical_polar_grid.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
ini_spherical_polar_grid.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_spherical_polar_grid.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_spherical_polar_grid.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_spherical_polar_grid.f: HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_spherical_polar_grid.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_spherical_polar_grid.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_spherical_polar_grid.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_spherical_polar_grid.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_spherical_polar_grid.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
ini_spherical_polar_grid.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h
ini_spherical_polar_grid.f: RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_spherical_polar_grid.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_theta.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_theta.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_theta.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_theta.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_theta.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_theta.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_theta.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_theta.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_theta.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_theta.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_theta.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
initialise_fixed.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
initialise_fixed.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
initialise_fixed.f: FCORI_MACROS.h
initialise_varia.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
initialise_varia.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
initialise_varia.f: FCORI_MACROS.h DYNVARS.h SURFACE.h
ini_vel.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_vel.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_vel.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_vel.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_vel.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_vel.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_vel.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_vel.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_vel.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_vel.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_vel.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_vertical_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_vertical_grid.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_vertical_grid.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_vertical_grid.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_vertical_grid.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_vertical_grid.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_vertical_grid.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_vertical_grid.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_vertical_grid.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_vertical_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_vertical_grid.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_vertical_grid.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
integrate_for_w.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
integrate_for_w.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
integrate_for_w.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
integrate_for_w.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
integrate_for_w.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
integrate_for_w.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
integrate_for_w.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
integrate_for_w.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
integrate_for_w.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
integrate_for_w.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
integrate_for_w.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
integrate_for_w.f: TANPHIATV_MACROS.h SURFACE.h
integr_continuity.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
integr_continuity.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
integr_continuity.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
integr_continuity.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
integr_continuity.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
integr_continuity.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
integr_continuity.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
integr_continuity.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
integr_continuity.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
integr_continuity.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
integr_continuity.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
integr_continuity.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
integr_continuity.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
integr_continuity.f: FFIELDS.h
load_fields_driver.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
load_fields_driver.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
load_fields_driver.f: PARAMS_MACROS.h FCORI_MACROS.h
load_grid_spacing.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
load_grid_spacing.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
load_grid_spacing.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
load_grid_spacing.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
load_grid_spacing.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
load_grid_spacing.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
load_grid_spacing.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
load_grid_spacing.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
load_grid_spacing.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
load_grid_spacing.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
load_grid_spacing.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
load_grid_spacing.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
load_ref_files.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
load_ref_files.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
modeldata_example.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
modeldata_example.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
momentum_correction_step.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
momentum_correction_step.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
momentum_correction_step.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
packages_boot.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
packages_boot.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
packages_boot.f: FCORI_MACROS.h
packages_check.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
packages_check.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
packages_check.f: FCORI_MACROS.h
packages_error_msg.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
packages_error_msg.f: EEPARAMS.h
packages_init_fixed.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
packages_init_fixed.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
packages_init_fixed.f: PARAMS_MACROS.h FCORI_MACROS.h
packages_init_variables.f: PACKAGES_CONFIG.h AD_CONFIG.h CPP_OPTIONS.h
packages_init_variables.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
packages_init_variables.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
packages_readparms.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
packages_readparms.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
packages_readparms.f: PARAMS_MACROS.h FCORI_MACROS.h
packages_write_pickup.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
packages_write_pickup.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
packages_write_pickup.f: PARAMS_MACROS.h FCORI_MACROS.h RESTART.h
plot_field.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
plot_field.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
post_cg3d.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
post_cg3d.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
post_cg3d.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
post_cg3d.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
post_cg3d.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
post_cg3d.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
post_cg3d.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
post_cg3d.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
post_cg3d.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
post_cg3d.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
post_cg3d.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h DYNVARS.h
post_cg3d.f: NH_VARS.h
pre_cg3d.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
pre_cg3d.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
pre_cg3d.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
pre_cg3d.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
pre_cg3d.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
pre_cg3d.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
pre_cg3d.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
pre_cg3d.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
pre_cg3d.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
pre_cg3d.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
pre_cg3d.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h FFIELDS.h
pre_cg3d.f: DYNVARS.h NH_VARS.h OBCS.h
pressure_for_eos.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
pressure_for_eos.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
pressure_for_eos.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
pressure_for_eos.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
pressure_for_eos.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
pressure_for_eos.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
pressure_for_eos.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
pressure_for_eos.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
pressure_for_eos.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
pressure_for_eos.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
pressure_for_eos.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
pressure_for_eos.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
read_pickup.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
read_pickup.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
read_pickup.f: RESTART.h DYNVARS.h SURFACE.h GAD.h NH_VARS.h
remove_mean.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
remove_mean.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
rotate_spherical_polar_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
rotate_spherical_polar_grid.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
rotate_spherical_polar_grid.f: FCORI_MACROS.h GRID.h GRID_MACROS.h
rotate_spherical_polar_grid.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
rotate_spherical_polar_grid.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
rotate_spherical_polar_grid.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
rotate_spherical_polar_grid.f: HFACS_MACROS.h HFACW_MACROS.h
rotate_spherical_polar_grid.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
rotate_spherical_polar_grid.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
rotate_spherical_polar_grid.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
rotate_spherical_polar_grid.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
rotate_spherical_polar_grid.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
rotate_spherical_polar_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
rotate_spherical_polar_grid.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
rotate_spherical_polar_grid.f: MASKW_MACROS.h MASKS_MACROS.h
rotate_spherical_polar_grid.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
rotate_uv2en.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h
rotate_uv2en.f: SIZE.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
rotate_uv2en.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
rotate_uv2en.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
rotate_uv2en.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
rotate_uv2en.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
rotate_uv2en.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
rotate_uv2en.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
rotate_uv2en.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
rotate_uv2en.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
rotate_uv2en.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
rotate_uv2en.f: TANPHIATV_MACROS.h
seawater.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
set_defaults.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
set_defaults.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
set_grid_factors.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
set_grid_factors.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
set_grid_factors.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
set_grid_factors.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
set_grid_factors.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
set_grid_factors.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
set_grid_factors.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
set_grid_factors.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
set_grid_factors.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
set_grid_factors.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
set_grid_factors.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
set_grid_factors.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
set_parms.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
set_parms.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h EOS.h
set_ref_state.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
set_ref_state.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
set_ref_state.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
set_ref_state.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
set_ref_state.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
set_ref_state.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
set_ref_state.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
set_ref_state.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
set_ref_state.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
set_ref_state.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
set_ref_state.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
set_ref_state.f: TANPHIATV_MACROS.h EOS.h
set_write_global_pickup.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
solve_for_pressure.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
solve_for_pressure.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
solve_for_pressure.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
solve_for_pressure.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
solve_for_pressure.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
solve_for_pressure.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
solve_for_pressure.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
solve_for_pressure.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
solve_for_pressure.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
solve_for_pressure.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
solve_for_pressure.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
solve_for_pressure.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
solve_for_pressure.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
solve_for_pressure.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
solve_for_pressure.f: SURFACE.h FFIELDS.h DYNVARS.h SOLVE_FOR_PRESSURE.h
solve_for_pressure.f: SOLVE_FOR_PRESSURE3D.h NH_VARS.h OBCS.h
solve_pentadiagonal.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
solve_pentadiagonal.f: EEPARAMS.h
solve_tridiagonal.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
solve_tridiagonal.f: EEPARAMS.h
state_summary.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
state_summary.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
state_summary.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
state_summary.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
state_summary.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
state_summary.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
state_summary.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
state_summary.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
state_summary.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
state_summary.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
state_summary.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
state_summary.f: TANPHIATV_MACROS.h DYNVARS.h
swfrac.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
taueddy_external_forcing.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
taueddy_external_forcing.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
taueddy_external_forcing.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h
taueddy_external_forcing.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
taueddy_external_forcing.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
taueddy_external_forcing.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
taueddy_external_forcing.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
taueddy_external_forcing.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
taueddy_external_forcing.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
taueddy_external_forcing.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
taueddy_external_forcing.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
taueddy_external_forcing.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
taueddy_external_forcing.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
taueddy_external_forcing.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
taueddy_external_forcing.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
taueddy_external_forcing.f: TANPHIATV_MACROS.h DYNVARS.h FFIELDS.h
taueddy_init_varia.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
taueddy_init_varia.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
taueddy_init_varia.f: FFIELDS.h
the_main_loop.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
the_main_loop.f: CPP_EEMACROS.h OBCS_OPTIONS.h SIZE.h EEPARAMS.h PARAMS.h
the_main_loop.f: PARAMS_MACROS.h FCORI_MACROS.h
the_model_main.f: AD_CONFIG.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
the_model_main.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
the_model_main.f: FCORI_MACROS.h DYNVARS.h
thermodynamics.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
thermodynamics.f: CPP_EEMACROS.h GAD_OPTIONS.h SIZE.h EEPARAMS.h PARAMS.h
thermodynamics.f: PARAMS_MACROS.h FCORI_MACROS.h RESTART.h DYNVARS.h GRID.h
thermodynamics.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
thermodynamics.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
thermodynamics.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
thermodynamics.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
thermodynamics.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
thermodynamics.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
thermodynamics.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
thermodynamics.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
thermodynamics.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
thermodynamics.f: TANPHIATV_MACROS.h GAD.h GAD_SOM_VARS.h TIMEAVE_STATV.h
thermodynamics.f: TIMEAVE_OPTIONS.h
timestep.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
timestep.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
timestep.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
timestep.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
timestep.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
timestep.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
timestep.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
timestep.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
timestep.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
timestep.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
timestep.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h RESTART.h
timestep.f: DYNVARS.h NH_VARS.h
timestep_tracer.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timestep_tracer.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
timestep_wvel.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
timestep_wvel.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
timestep_wvel.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
timestep_wvel.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
timestep_wvel.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
timestep_wvel.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
timestep_wvel.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
timestep_wvel.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
timestep_wvel.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
timestep_wvel.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
timestep_wvel.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
timestep_wvel.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h NH_VARS.h
tracers_correction_step.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
tracers_correction_step.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
tracers_correction_step.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
tracers_iigw_correction.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
tracers_iigw_correction.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
tracers_iigw_correction.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
tracers_iigw_correction.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
tracers_iigw_correction.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
tracers_iigw_correction.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
tracers_iigw_correction.f: HFACW_MACROS.h RECIP_DXC_MACROS.h
tracers_iigw_correction.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
tracers_iigw_correction.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
tracers_iigw_correction.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
tracers_iigw_correction.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
tracers_iigw_correction.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
tracers_iigw_correction.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h
tracers_iigw_correction.f: RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
tracers_iigw_correction.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
tracers_iigw_correction.f: NH_VARS.h
turnoff_model_io.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
turnoff_model_io.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
turnoff_model_io.f: FCORI_MACROS.h
update_cg2d.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
update_cg2d.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
update_cg2d.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
update_cg2d.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
update_cg2d.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
update_cg2d.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
update_cg2d.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
update_cg2d.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
update_cg2d.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
update_cg2d.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
update_cg2d.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
update_cg2d.f: TANPHIATV_MACROS.h SURFACE.h CG2D.h OBCS.h
update_etah.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
update_etah.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
update_etah.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
update_etah.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
update_etah.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
update_etah.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
update_etah.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
update_etah.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
update_etah.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
update_etah.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
update_etah.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
update_etah.f: TANPHIATV_MACROS.h SURFACE.h FFIELDS.h
update_masks_etc.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
update_masks_etc.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
update_masks_etc.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
update_masks_etc.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
update_masks_etc.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
update_masks_etc.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
update_masks_etc.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
update_masks_etc.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
update_masks_etc.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
update_masks_etc.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
update_masks_etc.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
update_masks_etc.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
update_r_star.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
update_r_star.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
update_r_star.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
update_r_star.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
update_r_star.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
update_r_star.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
update_r_star.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
update_r_star.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
update_r_star.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
update_r_star.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
update_r_star.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
update_r_star.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
update_sigma.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
update_sigma.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
update_sigma.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
update_sigma.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
update_sigma.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
update_sigma.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
update_sigma.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
update_sigma.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
update_sigma.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
update_sigma.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
update_sigma.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
update_sigma.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
update_surf_dr.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
update_surf_dr.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
update_surf_dr.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
update_surf_dr.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
update_surf_dr.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
update_surf_dr.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
update_surf_dr.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
update_surf_dr.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
update_surf_dr.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
update_surf_dr.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
update_surf_dr.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
update_surf_dr.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
update_surf_dr.f: TANPHIATV_MACROS.h SURFACE.h
write_grid.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
write_grid.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_grid.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
write_grid.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
write_grid.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
write_grid.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
write_grid.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
write_grid.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
write_grid.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
write_grid.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
write_grid.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
write_grid.f: TANPHIATV_MACROS.h
write_pickup.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
write_pickup.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
write_pickup.f: FCORI_MACROS.h RESTART.h DYNVARS.h SURFACE.h GAD.h NH_VARS.h
write_state.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
write_state.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_state.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
write_state.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
write_state.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
write_state.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
write_state.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
write_state.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
write_state.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
write_state.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
write_state.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
write_state.f: TANPHIATV_MACROS.h DYNVARS.h NH_VARS.h
