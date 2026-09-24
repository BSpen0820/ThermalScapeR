.chk_vec_len <- function(x, n, name) {
  if (length(x) != n)
    stop(sprintf("'%s' must have length %d (model_settings$julnum), got %d", name, n, length(x)))
}

#' Template for time-varying Endotherm model inputs
#'
#' Returns a named list covering every field the NicheMapR Endotherm model
#' exe supports as a "vary over the run" input, all defaulting to
#' \code{NULL}. Pass the result (with whichever fields you need overwritten
#' with a vector) as \code{\link{run_endo_big_nichemap}}'s \code{time_varying}
#' argument.
#'
#' @return A named list of 21 elements, all \code{NULL}:
#'   \describe{
#'     \item{Toggle-gated}{\code{mass_by_julday},
#'       \code{body_fat_pct_by_julday}, \code{core_temp_target_by_julday}
#'       (each backed by a static scalar + a \code{*_by_julday_enabled} flag
#'       elsewhere in \code{\link{get_endotherm_defaults}}'s output - see
#'       \code{\link{write_endotherm_inputs}});
#'       \code{torso_hair_length_dorsal_by_julday},
#'       \code{torso_hair_length_ventral_by_julday},
#'       \code{torso_fur_depth_dorsal_by_julday},
#'       \code{torso_fur_depth_ventral_by_julday} (torso hair length/fur depth,
#'       dorsal/ventral - these four share a single flag,
#'       \code{fur$torso_fur_by_julday_enabled}: supplying any one of them makes
#'       \code{\link{run_endo_big_nichemap}} treat all four as vectors,
#'       backfilling the ones you didn't supply with their static value).}
#'     \item{Always-vector}{\code{digestive_efficiency},
#'       \code{activity_basal_multiple}, \code{reproduction_basal_multiple},
#'       \code{food_protein_frac}, \code{food_fat_frac}, \code{food_carb_frac},
#'       \code{food_dry_matter_frac}, \code{diurnal_enabled},
#'       \code{nocturnal_enabled}, \code{crepuscular_enabled},
#'       \code{hibernate_enabled}, \code{hibernation_day_frac},
#'       \code{active_land_or_water}, \code{inactive_land_or_water} - already
#'       per-julday vectors by design in
#'       \code{\link{get_endotherm_defaults}}'s \code{diet} group; no flag
#'       is involved, a supplied vector simply overwrites the default
#'       repeated-constant vector.}
#'   }
#'
#' @seealso \code{\link{run_endo_big_nichemap}}, \code{\link{get_endotherm_defaults}}
#' @export
endo_timevar_template <- function() {
  list(
    mass_by_julday = NULL, body_fat_pct_by_julday = NULL, core_temp_target_by_julday = NULL,
    torso_hair_length_dorsal_by_julday = NULL, torso_hair_length_ventral_by_julday = NULL,
    torso_fur_depth_dorsal_by_julday = NULL, torso_fur_depth_ventral_by_julday = NULL,
    digestive_efficiency = NULL, activity_basal_multiple = NULL,
    reproduction_basal_multiple = NULL, food_protein_frac = NULL, food_fat_frac = NULL,
    food_carb_frac = NULL, food_dry_matter_frac = NULL, diurnal_enabled = NULL,
    nocturnal_enabled = NULL, crepuscular_enabled = NULL, hibernate_enabled = NULL,
    hibernation_day_frac = NULL, active_land_or_water = NULL, inactive_land_or_water = NULL
  )
}

#' Write NicheMapR Endotherm model input files (endo.dat, alomvars.dat)
#'
#' Builds the fixed-format \code{endo.dat} and \code{alomvars.dat} input files
#' required by the NicheMapR Endotherm model executable (\code{Endo2022a.exe}),
#' from grouped, defaulted, documented R arguments instead of a hand-edited
#' script. This is a direct port of \code{endo_alomvars_auto_V3.R}'s user-input
#' block and file-writing logic (Paul Mathewson, Megan Fitzpatrick, Warren
#' Porter) into a reusable package function.
#'
#' @param output_dir Directory to write \code{endo.dat} and \code{alomvars.dat}
#'   into. Written using these exact filenames (no \code{study_area} prefix),
#'   because \code{Endo2022a.exe} hard-codes these names in its working
#'   directory.
#' @param model_settings Named list of simulation-level settings: \code{julnum,
#'   juldays, hourly_output_enabled, output_file_enabled,
#'   microclimate_input_format, output_file_format, output_energy_units,
#'   metabolic_output_mode, stored_heat_enabled, body_geometry,
#'   geometry_axis_ratio, appendage_config, ventral_substrate_contact_frac,
#'   substrate_conduction_enabled, fur_compression_frac, user_allometry_enabled,
#'   activity_heat_enabled, thermoreg_trigger_tolerance, activity_hours_method,
#'   forage_rate_min, activity_heat_fraction, production_heat_fraction,
#'   ir_config_factor_sky, ir_config_factor_ground, ir_config_factor_objects,
#'   user_nu_re_enabled, nu_re_front_a, nu_re_front_b, nu_re_side_a,
#'   nu_re_side_b}. \code{juldays} must have length \code{julnum}.
#'   \code{metabolic_output_mode} is \code{endo.dat}'s DEPEND value (default
#'   \code{2.5}, matching the validated Female Bighorn Sheep baseline): if
#'   \code{2.0 < metabolic_output_mode < 3.0}, metabolic output is total W; if
#'   \code{metabolic_output_mode == 2.0}, it is W/kg.
#' @param animal Named list of whole-animal properties: \code{species_label,
#'   taxon_class, is_marsupial, specific_heat, body_mass,
#'   mass_by_julday_enabled, mass_by_julday, body_fat_pct,
#'   body_fat_pct_by_julday_enabled, body_fat_pct_by_julday,
#'   subcutaneous_fat_enabled, body_density, user_metabolic_rate_enabled,
#'   metabolic_rate}.
#' @param fur Named list of fur/feather properties: whole-body defaults
#'   (\code{per_part_fur_enabled, hair_diameter_dorsal, hair_diameter_ventral,
#'   hair_length_dorsal, hair_length_ventral, fur_depth_dorsal,
#'   fur_depth_ventral, hair_density_dorsal, hair_density_ventral,
#'   reflectivity_dorsal, reflectivity_ventral}), a \code{parts} sub-list whose
#'   elements (leg, head_neck, torso, tail for fur) each contain:
#'   \code{hair_diameter_dorsal, hair_diameter_ventral, hair_length_dorsal,
#'   hair_length_ventral, fur_depth_dorsal, fur_depth_ventral,
#'   hair_density_dorsal, hair_density_ventral, reflectivity_dorsal,
#'   reflectivity_ventral} used when \code{per_part_fur_enabled = 1}, and
#'   time-dependent torso fur (\code{torso_fur_by_julday_enabled,
#'   torso_hair_length_dorsal_by_julday, torso_hair_length_ventral_by_julday,
#'   torso_fur_depth_dorsal_by_julday, torso_fur_depth_ventral_by_julday}).
#' @param physiology Named list of core-temperature and heat-exchange
#'   physiology: \code{core_temp_target, core_temp_min, core_temp_max,
#'   hibernation_core_temp, core_temp_by_julday_enabled,
#'   core_temp_target_by_julday, core_skin_temp_diff_min,
#'   exhaled_air_temp_offset, skin_wetness_pct, skin_wetness_max_pct,
#'   sweating_enabled, piloerection_enabled, piloerection_max_pct,
#'   flesh_conductivity, flesh_conductivity_min, flesh_conductivity_max,
#'   user_fur_conductivity_enabled, user_fur_conductivity,
#'   radiant_exchange_fur_depth_frac, o2_extraction_max_pct,
#'   o2_extraction_min_pct}.
#' @param diet Named list of diet, digestion, and daily activity/hibernation
#'   schedule: \code{gut_passage_time_days, fecal_water_frac, urine_urea_frac,
#'   digestive_efficiency, activity_basal_multiple, reproduction_basal_multiple,
#'   food_protein_frac, food_fat_frac, food_carb_frac, food_dry_matter_frac,
#'   diurnal_enabled, nocturnal_enabled, crepuscular_enabled, hibernate_enabled,
#'   hibernation_day_frac, active_land_or_water, inactive_land_or_water}. The
#'   vector fields must have length \code{model_settings$julnum}.
#' @param thermoreg Named list of behavioral thermoregulation options:
#'   \code{burrow_enabled, nest_thermoreg_enabled, climb_enabled,
#'   shade_seeking_enabled, dive_cooling_enabled, wind_seeking_enabled,
#'   night_shade_enabled, dive_option_enabled, active_in_shade_enabled,
#'   shade_posture, behavior_first_enabled, burrow_nest_use_option,
#'   wade_enabled, tree_sleep_enabled, tree_sleep_shade_pct, huddle_enabled,
#'   huddle_group_size, huddle_contact_dorsal_frac, huddle_contact_ventral_frac,
#'   concurrent_core_temp_increase_enabled, core_temp_change_increment,
#'   core_temp_water_loss_trigger, o2_extraction_increment_pct,
#'   skin_wetness_increment_pct, concurrent_core_temp_decrease_enabled,
#'   piloerection_increment_frac}.
#' @param flying_digging Named list of flight and burrowing/fossoriality
#'   options: \code{flight_enabled, flight_metabolic_rate, flight_velocity,
#'   flight_load, fossorial_enabled, digging_enabled, fossorial_node,
#'   arboreal_enabled, burrow_o2_pct, burrow_co2_pct, burrow_n2_pct, soil_type,
#'   burrow_segment_length, burrow_depth}.
#' @param nest_shelter Named list of nest/shelter geometry and use:
#'   \code{shelter_type, nest_use_when_inactive_enabled, nest_wall_thickness,
#'   nest_wall_conductivity, nest_occupants, nest_location, nest_soil_node,
#'   shelter_length, shelter_outer_diameter, shelter_solar_reflectivity,
#'   shelter_transient_enabled, shelter_material_density,
#'   shelter_material_specific_heat, shelter_height_rel_ground,
#'   shelter_node_count, shelter_node_radii}.
#' @param allometry Named list used only to build \code{alomvars.dat}:
#'   \code{taxon_group, locomotion}, a \code{parts} sub-list whose elements
#'   (head, neck, torso, front_leg, rear_leg, tail for allometry) each contain:
#'   \code{diameter_vertical, diameter_horizontal, length,
#'   fur_depth_dorsal_reference, fur_depth_dorsal_adjusted,
#'   fur_depth_ventral_reference, fur_depth_ventral_adjusted, density,
#'   geometry}, \code{sixth_appendage_type, absolute_measurement_cm,
#'   absolute_measurement_type, dimension_adjustment}, per-part subcutaneous fat
#'   flags (\code{subcutaneous_fat_head_enabled, subcutaneous_fat_neck_enabled,
#'   subcutaneous_fat_torso_enabled, subcutaneous_fat_front_leg_enabled,
#'   subcutaneous_fat_rear_leg_enabled, subcutaneous_fat_tail_enabled,
#'   mass_change_source}), inactive postures (\code{posture_1_enabled,
#'   posture_2_enabled, posture_3_enabled, posture_4_enabled,
#'   sleep_start_posture, shade_start_posture, inactive_end_posture}), per-part
#'   minimum flesh conductivity (\code{flesh_conductivity_min_head,
#'   flesh_conductivity_min_neck, flesh_conductivity_min_torso,
#'   flesh_conductivity_min_front_leg, flesh_conductivity_min_rear_leg,
#'   flesh_conductivity_min_tail, variable_core_temp_legs_enabled,
#'   variable_core_temp_sixth_appendage_enabled,
#'   sixth_appendage_on_ground_enabled}), leg-shading geometry
#'   (\code{torso_overhang, leg_vertical_offset}), bird sleeping
#'   (\code{bird_sleep_standing_enabled, bird_sleep_leg_count}), and
#'   countercurrent exchange (\code{core_temp_reduction_legs_enabled,
#'   core_temp_reduction_sixth_appendage_enabled,
#'   core_temp_reduction_legs_mode, core_temp_reduction_sixth_appendage_mode,
#'   core_temp_reduction_legs_fraction,
#'   core_temp_reduction_sixth_appendage_fraction,
#'   core_temp_reduction_legs_difference,
#'   core_temp_reduction_sixth_appendage_difference, appendage_temp_min,
#'   leg_temp_increase_if_hot_enabled,
#'   sixth_appendage_temp_increase_if_hot_enabled}). Always written
#'   regardless of \code{model_settings$user_allometry_enabled}, matching the source
#'   script's behavior.
#' @param study_area Optional string recorded in the returned log only; it
#'   does not prefix the output filenames (see \code{output_dir}).
#'
#' @return Invisibly, a log \code{data.frame} with columns \code{file_path,
#'   step, status, timestamp}, one row per file written.
#'
#' @details
#' The row-by-row text assembly mirrors the source script's \code{paste()}/
#' \code{format()}/\code{sQuote()} calls exactly, including tab/space padding,
#' because \code{Endo2022a.exe} is a compiled reader that is presumed to parse
#' these fixed-format files by column position. Two quirks of the source
#' script are preserved for fidelity rather than silently fixed:
#' \itemize{
#'   \item \code{thermoreg$core_temp_change_increment} is used for both the sweating and
#'     piloerection concurrent-temperature-change increments (the source
#'     script assigns two same-named variables, so only one value ever
#'     reaches the file).
#'   \item In \code{alomvars.dat}'s 6th-appendage (tail/proboscis) row, the
#'     locomotion type (\code{allometry$locomotion}) is written a second time
#'     in the column documented as tail-vs-proboscis type;
#'     \code{allometry$sixth_appendage_type} is accepted but not currently written anywhere, matching the source
#'     script exactly.
#' }
#'
#' The abbreviated pre-rename field names (for example \code{mass} or
#' \code{tcreg}) are no longer recognised. Because each group list is merged
#' onto the defaults, an unrecognised field name is silently ignored and the
#' default value is written instead, so scripts written against the old names
#' must be updated using the old -> new tables below.
#'
#' Field names use readable snake_case; the abbreviated names used by the
#' NicheMapR Endotherm script map as follows (field meanings match the column
#' headers written into \code{endo.dat}/\code{alomvars.dat}):
#'
#' \strong{model_settings}
#'   \tabular{ll}{
#'     \code{hrout} \tab \code{hourly_output_enabled} \cr
#'     \code{outout} \tab \code{output_file_enabled} \cr
#'     \code{microin} \tab \code{microclimate_input_format} \cr
#'     \code{outfile} \tab \code{output_file_format} \cr
#'     \code{outunits} \tab \code{output_energy_units} \cr
#'     \code{depend} \tab \code{metabolic_output_mode} \cr
#'     \code{strht} \tab \code{stored_heat_enabled} \cr
#'     \code{geom} \tab \code{body_geometry} \cr
#'     \code{geomult} \tab \code{geometry_axis_ratio} \cr
#'     \code{apnd} \tab \code{appendage_config} \cr
#'     \code{ventpct} \tab \code{ventral_substrate_contact_frac} \cr
#'     \code{inccond} \tab \code{substrate_conduction_enabled} \cr
#'     \code{frcmpr} \tab \code{fur_compression_frac} \cr
#'     \code{usralom} \tab \code{user_allometry_enabled} \cr
#'     \code{actht} \tab \code{activity_heat_enabled} \cr
#'     \code{err} \tab \code{thermoreg_trigger_tolerance} \cr
#'     \code{acthrs} \tab \code{activity_hours_method} \cr
#'     \code{minfrg} \tab \code{forage_rate_min} \cr
#'     \code{nrght} \tab \code{activity_heat_fraction} \cr
#'     \code{prdht} \tab \code{production_heat_fraction} \cr
#'     \code{fasky} \tab \code{ir_config_factor_sky} \cr
#'     \code{fagrd} \tab \code{ir_config_factor_ground} \cr
#'     \code{faobj} \tab \code{ir_config_factor_objects} \cr
#'     \code{usrnure} \tab \code{user_nu_re_enabled} \cr
#'     \code{afrnt} \tab \code{nu_re_front_a} \cr
#'     \code{bfrnt} \tab \code{nu_re_front_b} \cr
#'     \code{aside} \tab \code{nu_re_side_a} \cr
#'     \code{bside} \tab \code{nu_re_side_b} \cr
#'   }
#'
#' \strong{animal}
#'   \tabular{ll}{
#'     \code{species} \tab \code{species_label} \cr
#'     \code{class} \tab \code{taxon_class} \cr
#'     \code{marsup} \tab \code{is_marsupial} \cr
#'     \code{cp} \tab \code{specific_heat} \cr
#'     \code{mass} \tab \code{body_mass} \cr
#'     \code{timdepmass} \tab \code{mass_by_julday_enabled} \cr
#'     \code{mass2} \tab \code{mass_by_julday} \cr
#'     \code{fatpct} \tab \code{body_fat_pct} \cr
#'     \code{timdepfat} \tab \code{body_fat_pct_by_julday_enabled} \cr
#'     \code{fatpct2} \tab \code{body_fat_pct_by_julday} \cr
#'     \code{subqfat} \tab \code{subcutaneous_fat_enabled} \cr
#'     \code{density} \tab \code{body_density} \cr
#'     \code{usrmet} \tab \code{user_metabolic_rate_enabled} \cr
#'     \code{met} \tab \code{metabolic_rate} \cr
#'   }
#'
#' \strong{fur}
#'   \tabular{ll}{
#'     \code{diad} \tab \code{hair_diameter_dorsal} \cr
#'     \code{diav} \tab \code{hair_diameter_ventral} \cr
#'     \code{lend} \tab \code{hair_length_dorsal} \cr
#'     \code{lenv} \tab \code{hair_length_ventral} \cr
#'     \code{depd} \tab \code{fur_depth_dorsal} \cr
#'     \code{depv} \tab \code{fur_depth_ventral} \cr
#'     \code{dend} \tab \code{hair_density_dorsal} \cr
#'     \code{denv} \tab \code{hair_density_ventral} \cr
#'     \code{refld} \tab \code{reflectivity_dorsal} \cr
#'     \code{reflv} \tab \code{reflectivity_ventral} \cr
#'     \code{varfur} \tab \code{per_part_fur_enabled} \cr
#'     \code{tmdptorfur} \tab \code{torso_fur_by_julday_enabled} \cr
#'     \code{torlend} \tab \code{torso_hair_length_dorsal_by_julday} \cr
#'     \code{torlenv} \tab \code{torso_hair_length_ventral_by_julday} \cr
#'     \code{tordepd} \tab \code{torso_fur_depth_dorsal_by_julday} \cr
#'     \code{tordepv} \tab \code{torso_fur_depth_ventral_by_julday} \cr
#'     \code{parts.<part>.diad} \tab \code{parts.<part>.hair_diameter_dorsal} \cr
#'     \code{parts.<part>.diav} \tab \code{parts.<part>.hair_diameter_ventral} \cr
#'     \code{parts.<part>.lend} \tab \code{parts.<part>.hair_length_dorsal} \cr
#'     \code{parts.<part>.lenv} \tab \code{parts.<part>.hair_length_ventral} \cr
#'     \code{parts.<part>.depd} \tab \code{parts.<part>.fur_depth_dorsal} \cr
#'     \code{parts.<part>.depv} \tab \code{parts.<part>.fur_depth_ventral} \cr
#'     \code{parts.<part>.dend} \tab \code{parts.<part>.hair_density_dorsal} \cr
#'     \code{parts.<part>.denv} \tab \code{parts.<part>.hair_density_ventral} \cr
#'     \code{parts.<part>.refld} \tab \code{parts.<part>.reflectivity_dorsal} \cr
#'     \code{parts.<part>.reflv} \tab \code{parts.<part>.reflectivity_ventral} \cr
#'   }
#'
#' \strong{physiology}
#'   \tabular{ll}{
#'     \code{tcreg} \tab \code{core_temp_target} \cr
#'     \code{tcmin} \tab \code{core_temp_min} \cr
#'     \code{tcmax} \tab \code{core_temp_max} \cr
#'     \code{tchib} \tab \code{hibernation_core_temp} \cr
#'     \code{tmdptc} \tab \code{core_temp_by_julday_enabled} \cr
#'     \code{tcreg2} \tab \code{core_temp_target_by_julday} \cr
#'     \code{tctskdif} \tab \code{core_skin_temp_diff_min} \cr
#'     \code{texptair} \tab \code{exhaled_air_temp_offset} \cr
#'     \code{sknwet} \tab \code{skin_wetness_pct} \cr
#'     \code{maxsknwet} \tab \code{skin_wetness_max_pct} \cr
#'     \code{sweat} \tab \code{sweating_enabled} \cr
#'     \code{pilo} \tab \code{piloerection_enabled} \cr
#'     \code{maxpilo} \tab \code{piloerection_max_pct} \cr
#'     \code{flshk} \tab \code{flesh_conductivity} \cr
#'     \code{flshkmin} \tab \code{flesh_conductivity_min} \cr
#'     \code{flshkmax} \tab \code{flesh_conductivity_max} \cr
#'     \code{usrfurk} \tab \code{user_fur_conductivity_enabled} \cr
#'     \code{usrfurk2} \tab \code{user_fur_conductivity} \cr
#'     \code{radfurdep} \tab \code{radiant_exchange_fur_depth_frac} \cr
#'     \code{o2max} \tab \code{o2_extraction_max_pct} \cr
#'     \code{o2min} \tab \code{o2_extraction_min_pct} \cr
#'   }
#'
#' \strong{diet}
#'   \tabular{ll}{
#'     \code{gut} \tab \code{gut_passage_time_days} \cr
#'     \code{fech2o} \tab \code{fecal_water_frac} \cr
#'     \code{urea} \tab \code{urine_urea_frac} \cr
#'     \code{digef} \tab \code{digestive_efficiency} \cr
#'     \code{act} \tab \code{activity_basal_multiple} \cr
#'     \code{repro} \tab \code{reproduction_basal_multiple} \cr
#'     \code{prtn} \tab \code{food_protein_frac} \cr
#'     \code{fat} \tab \code{food_fat_frac} \cr
#'     \code{carb} \tab \code{food_carb_frac} \cr
#'     \code{dry} \tab \code{food_dry_matter_frac} \cr
#'     \code{diurn} \tab \code{diurnal_enabled} \cr
#'     \code{noct} \tab \code{nocturnal_enabled} \cr
#'     \code{crep} \tab \code{crepuscular_enabled} \cr
#'     \code{hibrn} \tab \code{hibernate_enabled} \cr
#'     \code{hibfrac} \tab \code{hibernation_day_frac} \cr
#'     \code{land} \tab \code{active_land_or_water} \cr
#'     \code{land2} \tab \code{inactive_land_or_water} \cr
#'   }
#'
#' \strong{thermoreg}
#'   \tabular{ll}{
#'     \code{burrow} \tab \code{burrow_enabled} \cr
#'     \code{nest} \tab \code{nest_thermoreg_enabled} \cr
#'     \code{climb} \tab \code{climb_enabled} \cr
#'     \code{shdseek} \tab \code{shade_seeking_enabled} \cr
#'     \code{dive} \tab \code{dive_cooling_enabled} \cr
#'     \code{wind} \tab \code{wind_seeking_enabled} \cr
#'     \code{niteshd} \tab \code{night_shade_enabled} \cr
#'     \code{dive2} \tab \code{dive_option_enabled} \cr
#'     \code{shdact} \tab \code{active_in_shade_enabled} \cr
#'     \code{shdpost} \tab \code{shade_posture} \cr
#'     \code{trord} \tab \code{behavior_first_enabled} \cr
#'     \code{burTR} \tab \code{burrow_nest_use_option} \cr
#'     \code{wade} \tab \code{wade_enabled} \cr
#'     \code{treeslp} \tab \code{tree_sleep_enabled} \cr
#'     \code{slpcnpy} \tab \code{tree_sleep_shade_pct} \cr
#'     \code{hudl} \tab \code{huddle_enabled} \cr
#'     \code{hudlnum} \tab \code{huddle_group_size} \cr
#'     \code{hudldrs} \tab \code{huddle_contact_dorsal_frac} \cr
#'     \code{hudlvnt} \tab \code{huddle_contact_ventral_frac} \cr
#'     \code{tcconcur} \tab \code{concurrent_core_temp_increase_enabled} \cr
#'     \code{tcinc} \tab \code{core_temp_change_increment} \cr
#'     \code{tcwtr} \tab \code{core_temp_water_loss_trigger} \cr
#'     \code{o2inc} \tab \code{o2_extraction_increment_pct} \cr
#'     \code{sknwtinc} \tab \code{skin_wetness_increment_pct} \cr
#'     \code{tcconcur2} \tab \code{concurrent_core_temp_decrease_enabled} \cr
#'     \code{piloinc} \tab \code{piloerection_increment_frac} \cr
#'   }
#'
#' \strong{flying_digging}
#'   \tabular{ll}{
#'     \code{flight} \tab \code{flight_enabled} \cr
#'     \code{fltmetab} \tab \code{flight_metabolic_rate} \cr
#'     \code{fltvel} \tab \code{flight_velocity} \cr
#'     \code{fltload} \tab \code{flight_load} \cr
#'     \code{foss} \tab \code{fossorial_enabled} \cr
#'     \code{dig} \tab \code{digging_enabled} \cr
#'     \code{nodes} \tab \code{fossorial_node} \cr
#'     \code{arb} \tab \code{arboreal_enabled} \cr
#'     \code{buro2} \tab \code{burrow_o2_pct} \cr
#'     \code{burco2} \tab \code{burrow_co2_pct} \cr
#'     \code{burn2} \tab \code{burrow_n2_pct} \cr
#'     \code{soiltyp} \tab \code{soil_type} \cr
#'     \code{burseg} \tab \code{burrow_segment_length} \cr
#'     \code{burdep} \tab \code{burrow_depth} \cr
#'   }
#'
#' \strong{nest_shelter}
#'   \tabular{ll}{
#'     \code{shelter} \tab \code{shelter_type} \cr
#'     \code{nestuse} \tab \code{nest_use_when_inactive_enabled} \cr
#'     \code{nestthk} \tab \code{nest_wall_thickness} \cr
#'     \code{nestk} \tab \code{nest_wall_conductivity} \cr
#'     \code{nestno} \tab \code{nest_occupants} \cr
#'     \code{nestloc} \tab \code{nest_location} \cr
#'     \code{nestnode} \tab \code{nest_soil_node} \cr
#'     \code{nestlength} \tab \code{shelter_length} \cr
#'     \code{outdiam} \tab \code{shelter_outer_diameter} \cr
#'     \code{shltrefl} \tab \code{shelter_solar_reflectivity} \cr
#'     \code{shltrans} \tab \code{shelter_transient_enabled} \cr
#'     \code{shltdens} \tab \code{shelter_material_density} \cr
#'     \code{shltcp} \tab \code{shelter_material_specific_heat} \cr
#'     \code{nestheight} \tab \code{shelter_height_rel_ground} \cr
#'     \code{shltnodes} \tab \code{shelter_node_count} \cr
#'     \code{noderadius} \tab \code{shelter_node_radii} \cr
#'   }
#'
#' \strong{allometry}
#'   \tabular{ll}{
#'     \code{group} \tab \code{taxon_group} \cr
#'     \code{loco} \tab \code{locomotion} \cr
#'     \code{tail_type} \tab \code{sixth_appendage_type} \cr
#'     \code{absval} \tab \code{absolute_measurement_cm} \cr
#'     \code{absdim} \tab \code{absolute_measurement_type} \cr
#'     \code{adjdim} \tab \code{dimension_adjustment} \cr
#'     \code{subq_head} \tab \code{subcutaneous_fat_head_enabled} \cr
#'     \code{subq_neck} \tab \code{subcutaneous_fat_neck_enabled} \cr
#'     \code{subq_torso} \tab \code{subcutaneous_fat_torso_enabled} \cr
#'     \code{subq_front_leg} \tab \code{subcutaneous_fat_front_leg_enabled} \cr
#'     \code{subq_rear_leg} \tab \code{subcutaneous_fat_rear_leg_enabled} \cr
#'     \code{subq_tail} \tab \code{subcutaneous_fat_tail_enabled} \cr
#'     \code{tmdpfat} \tab \code{mass_change_source} \cr
#'     \code{post1} \tab \code{posture_1_enabled} \cr
#'     \code{post2} \tab \code{posture_2_enabled} \cr
#'     \code{post3} \tab \code{posture_3_enabled} \cr
#'     \code{post4} \tab \code{posture_4_enabled} \cr
#'     \code{slpstrt} \tab \code{sleep_start_posture} \cr
#'     \code{shdstrt} \tab \code{shade_start_posture} \cr
#'     \code{endpost} \tab \code{inactive_end_posture} \cr
#'     \code{akmin_head} \tab \code{flesh_conductivity_min_head} \cr
#'     \code{akmin_neck} \tab \code{flesh_conductivity_min_neck} \cr
#'     \code{akmin_torso} \tab \code{flesh_conductivity_min_torso} \cr
#'     \code{akmin_front_leg} \tab \code{flesh_conductivity_min_front_leg} \cr
#'     \code{akmin_rear_leg} \tab \code{flesh_conductivity_min_rear_leg} \cr
#'     \code{akmin_tail} \tab \code{flesh_conductivity_min_tail} \cr
#'     \code{VTleg} \tab \code{variable_core_temp_legs_enabled} \cr
#'     \code{VT6th} \tab \code{variable_core_temp_sixth_appendage_enabled} \cr
#'     \code{Grd6th} \tab \code{sixth_appendage_on_ground_enabled} \cr
#'     \code{torsover} \tab \code{torso_overhang} \cr
#'     \code{torsoff} \tab \code{leg_vertical_offset} \cr
#'     \code{brdslp} \tab \code{bird_sleep_standing_enabled} \cr
#'     \code{brdslplg} \tab \code{bird_sleep_leg_count} \cr
#'     \code{Tlegred} \tab \code{core_temp_reduction_legs_enabled} \cr
#'     \code{T6thred} \tab \code{core_temp_reduction_sixth_appendage_enabled} \cr
#'     \code{Tcred1} \tab \code{core_temp_reduction_legs_mode} \cr
#'     \code{Tcred2} \tab \code{core_temp_reduction_sixth_appendage_mode} \cr
#'     \code{Tfrac1} \tab \code{core_temp_reduction_legs_fraction} \cr
#'     \code{Tfrac2} \tab \code{core_temp_reduction_sixth_appendage_fraction} \cr
#'     \code{Tdif1} \tab \code{core_temp_reduction_legs_difference} \cr
#'     \code{Tdif2} \tab \code{core_temp_reduction_sixth_appendage_difference} \cr
#'     \code{MinT} \tab \code{appendage_temp_min} \cr
#'     \code{Tleginc} \tab \code{leg_temp_increase_if_hot_enabled} \cr
#'     \code{T6thinc} \tab \code{sixth_appendage_temp_increase_if_hot_enabled} \cr
#'     \code{parts.<part>.diav} \tab \code{parts.<part>.diameter_vertical} \cr
#'     \code{parts.<part>.diah} \tab \code{parts.<part>.diameter_horizontal} \cr
#'     \code{parts.<part>.len} \tab \code{parts.<part>.length} \cr
#'     \code{parts.<part>.dorsfur} \tab \code{parts.<part>.fur_depth_dorsal_reference} \cr
#'     \code{parts.<part>.dorsfur_a} \tab \code{parts.<part>.fur_depth_dorsal_adjusted} \cr
#'     \code{parts.<part>.ventfur} \tab \code{parts.<part>.fur_depth_ventral_reference} \cr
#'     \code{parts.<part>.ventfur_a} \tab \code{parts.<part>.fur_depth_ventral_adjusted} \cr
#'     \code{parts.<part>.dens} \tab \code{parts.<part>.density} \cr
#'     \code{parts.<part>.geom} \tab \code{parts.<part>.geometry} \cr
#'   }
#'
#' Running \code{Endo2022a.exe}, parsing its outputs, and generating
#' \code{JULDAYS.dat} are out of scope for this function.
#'
#' @export
write_endotherm_inputs <- function(output_dir,
                                    model_settings = list(),
                                    animal         = list(),
                                    fur            = list(),
                                    physiology     = list(),
                                    diet           = list(),
                                    thermoreg      = list(),
                                    flying_digging = list(),
                                    nest_shelter   = list(),
                                    allometry      = list(),
                                    study_area     = NULL) {

  if (!dir.exists(output_dir))
    stop(sprintf("'output_dir' does not exist:\n  %s", output_dir))

  ms0 <- utils::modifyList(.endo_baseline()$model_settings, model_settings)
  julnum <- ms0$julnum
  .chk_vec_len(ms0$juldays, julnum, "model_settings$juldays")

  base <- .endo_baseline(julnum, ms0$juldays)
  ms <- utils::modifyList(base$model_settings, model_settings)
  an <- utils::modifyList(base$animal, animal)
  fr <- utils::modifyList(base$fur, fur)
  ph <- utils::modifyList(base$physiology, physiology)
  di <- utils::modifyList(base$diet, diet)
  tr <- utils::modifyList(base$thermoreg, thermoreg)
  fd <- utils::modifyList(base$flying_digging, flying_digging)
  ns <- utils::modifyList(base$nest_shelter, nest_shelter)
  al <- utils::modifyList(base$allometry, allometry)

  for (.v in list(list(an$mass_by_julday, "animal$mass_by_julday"), list(an$body_fat_pct_by_julday, "animal$body_fat_pct_by_julday"),
                  list(ph$core_temp_target_by_julday, "physiology$core_temp_target_by_julday"),
                  list(fr$torso_hair_length_dorsal_by_julday, "fur$torso_hair_length_dorsal_by_julday"), list(fr$torso_hair_length_ventral_by_julday, "fur$torso_hair_length_ventral_by_julday"),
                  list(fr$torso_fur_depth_dorsal_by_julday, "fur$torso_fur_depth_dorsal_by_julday"), list(fr$torso_fur_depth_ventral_by_julday, "fur$torso_fur_depth_ventral_by_julday"),
                  list(di$digestive_efficiency, "diet$digestive_efficiency"), list(di$activity_basal_multiple, "diet$activity_basal_multiple"),
                  list(di$reproduction_basal_multiple, "diet$reproduction_basal_multiple"), list(di$food_protein_frac, "diet$food_protein_frac"),
                  list(di$food_fat_frac, "diet$food_fat_frac"), list(di$food_carb_frac, "diet$food_carb_frac"),
                  list(di$food_dry_matter_frac, "diet$food_dry_matter_frac"), list(di$diurnal_enabled, "diet$diurnal_enabled"),
                  list(di$nocturnal_enabled, "diet$nocturnal_enabled"), list(di$crepuscular_enabled, "diet$crepuscular_enabled"),
                  list(di$hibernate_enabled, "diet$hibernate_enabled"), list(di$hibernation_day_frac, "diet$hibernation_day_frac"),
                  list(di$active_land_or_water, "diet$active_land_or_water"), list(di$inactive_land_or_water, "diet$inactive_land_or_water")))
    .chk_vec_len(.v[[1]], julnum, .v[[2]])

  old_fancy <- getOption("useFancyQuotes")
  options(useFancyQuotes = FALSE)
  on.exit(options(useFancyQuotes = old_fancy), add = TRUE)

  fur_leg  <- if (fr$per_part_fur_enabled == 0) fr else fr$parts$leg
  fur_hn   <- if (fr$per_part_fur_enabled == 0) fr else fr$parts$head_neck
  fur_trs  <- if (fr$per_part_fur_enabled == 0) fr else fr$parts$torso
  fur_tail <- if (fr$per_part_fur_enabled == 0) fr else fr$parts$tail

  # NOTE: the "legs" row uses different spacing (two spaces before
  # fur_depth_dorsal and hair_density_ventral) than the head&neck/torso/tail
  # rows (tab before fur_depth_dorsal, one space before hair_density_ventral)
  # in the source script. Preserved verbatim as two separate builders.
  .fur_row_leg <- function(p) {
    c(format(round(p$hair_diameter_dorsal, 1), nsmall = 1), " ", format(round(p$hair_diameter_ventral, 1), nsmall = 1), " ",
      format(round(p$hair_length_dorsal, 1), nsmall = 1), " ", format(round(p$hair_length_ventral, 1), nsmall = 1), "  ",
      format(round(p$fur_depth_dorsal, 1), nsmall = 1), " ", format(round(p$fur_depth_ventral, 1), nsmall = 1), "    ",
      format(round(p$hair_density_dorsal, 1), nsmall = 1), "  ", format(round(p$hair_density_ventral, 1), nsmall = 1), "  ",
      format(round(p$reflectivity_ventral, 3), nsmall = 2), "  ", format(round(p$reflectivity_dorsal, 3), nsmall = 2), "\t   ", 0.08, "\n")
  }
  .fur_row_other <- function(p) {
    c(format(round(p$hair_diameter_dorsal, 1), nsmall = 1), " ", format(round(p$hair_diameter_ventral, 1), nsmall = 1), " ",
      format(round(p$hair_length_dorsal, 1), nsmall = 1), " ", format(round(p$hair_length_ventral, 1), nsmall = 1), "\t",
      format(round(p$fur_depth_dorsal, 1), nsmall = 1), " ", format(round(p$fur_depth_ventral, 1), nsmall = 1), "    ",
      format(round(p$hair_density_dorsal, 1), nsmall = 1), " ", format(round(p$hair_density_ventral, 1), nsmall = 1), "  ",
      format(round(p$reflectivity_ventral, 3), nsmall = 2), "  ", format(round(p$reflectivity_dorsal, 3), nsmall = 2), "\t   ", 0.08, "\n")
  }

  # --- Begin building the endo.dat file --------------------------------------
  row1  <- c("Name simulation & input file to run: 'endoprop','endotime', and 'endosens' are choices. Input for Endo2011a.", "\n")
  row2  <- c("endoprop = no variation in animal parameters with time; read only this file", "\n")
  row3  <- c("\"endotime = time dependent variables, e.g. core temp., food type available, etc.; \"", "\n")
  row4  <- c("endosens = sensitivity analysis for different body sizes for given climate regime;", "\n")
  row5  <- c("2ND VARIABLE: Hourly output (y/n)? Do not use for GIS-type calc's unless need hourly output", "\n")
  row6  <- c("Hourly output = 'y' creates files 'HOURPLOT.OUT' and 'ACTHOURS.OUT'.3RD VAR = 'y'->file OUTPUT printed", "\n")
  row7  <- c("\"4TH VAR: IF 2.0<DEPEND<3.0 = total metab.in W;if DEPEND=2.0, then metab. in W/kg;\"", "\n")
  row8  <- c("5TH-7TH variables: 'ECTHRM' vs 'NDTHRM' (Ectotherm vs. Endotherm)=known [Tc-Met rate known; solve for Tc] vs", "\n")
  row9  <- c("\"[fixed Tc, solve for Met(no flight) OR [fixed Met, solve for Tc (flight). USE 'NDTHRM' IF FLIGHT = 'Y'\"", "\n")
  row10 <- c("\"If 'ECTHRM', then read slope & intercept of ln(ml O2/g/h) = slope*Tc + intercept, ELSE USE ZERO FOR SLOPE AND INTERCEPT IF NDTHRM\"", "\n")
  row11 <- c("8th Variable: What type of microclimate input files are read in. 'CSV' if using Micro2011; 'OUT' if using Micro2010", "\n")
  row12 <- c("9th Variable: What type of output files from endotherm model: 'CSV' or 'OUT'", "\n")
  row13 <- c("10th Variable: What units for metabolic requirements in MONTH and YEAR output files: 'JL', 'KJ', or 'MJ'  *Joules, Kilojoules, or Megajoules*", "\n")
  row14 <- c("-----------------------------------------------------------------------------", "\n")
  row15 <- paste("'ENDOTIME' '", ms$hourly_output_enabled, "' '", ms$output_file_enabled, "' ", ms$metabolic_output_mode, " 'NDTHRM' 0 0 '", ms$microclimate_input_format, "'  '", ms$output_file_format, "' '", ms$output_energy_units, "'", sep = "", "\n")
  row16 <- c("", "\n")
  row17 <- c("Do a transient in   If a transient, is it   Consider stored  Specific   Class of animal (6 letters): 'MAMMAL',\"\t", "\n")
  row18 <- c("addition to steady  for the animal (1.)  heat in energy   Heat       'BIRDIE','REPTIL','AMPHIB','INSECT'\"\t\t  ", "\n")
  row19 <- c("state (y/n)         or nest/shelter (0.)?   balance?(Y/n)    (J/KgC)    mammal,bird,reptile,amphibian,insect)\"\t   MARSUPIAL?", "\n")
  row20 <- c("--------            -------------------     -------------    -------    ----------------------------------------    ----------", "\n")
  row21 <- paste("'N'                  1                      '", ms$stored_heat_enabled, "'               ", an$specific_heat, "       '", an$taxon_class, "'                                    '", an$is_marsupial, "'", sep = "", "\n")
  row22 <- c("", "\n")
  row23 <- paste("Animal species = ", an$species_label, sep = "", "\n")
  row24 <- c("Animal Variables           Dep var form: 2.0<DEPEND<3.0 = total metab.in W", "\n")
  row25 <- c("ALLOMETRIC properties ", "\n")
  row26 <- c("Geometric properties\t\t     Whole body(torso if apndgs) Geom Mult      '0APNDG'if no appendages-IN CAPS!!\t\t", "\n")
  row27 <- c("Max       Fat mass   Is fat subcut?    Geometric approx.(integer)  Ellips:(A:B)   '2APNDG'if 2 appndg (e.g.bird)   % ventral area      Include\t\t  dec % fur   Animal \t    User supplied\t", "\n")
  row28 <- c("\"weight   as % body  (If so, affects     1=cyl,2=spher,             Cyl:(L:Rskin)  '4APNDG'if 4 appndg (e.g.mammal) contacting substr.  conduction\t  compression density \t    allometry?", "\n")
  row29 <- c("\"(kg)     mass(%)    heat loss)(Y/N)     4=ellipsoid                                  use complx geom's              (decimal 100%=1.O) w/ sub? (Y/N)      for condct  kg/m3(932.9)    (Y/N)", "\n")
  row30 <- c("------    ----       -------------      ----------------------     ---------      ----------------                 ----------\t         ------------        -----\t  --------     --------------", "\n")
  row31 <- c(format(round(an$body_mass, 2), nsmall = 2), "\t   ", format(round(an$body_fat_pct, 2), nsmall = 2), "\t\t  ", sQuote(an$subcutaneous_fat_enabled), "\t\t\t", ms$body_geometry, "\t\t   ", format(round(ms$geometry_axis_ratio, 2), nsmall = 2), "\t\t    ", sQuote(ms$appendage_config), "\t\t\t\t", format(round(ms$ventral_substrate_contact_frac, 2), nsmall = 2), "\t\t   ", sQuote(ms$substrate_conduction_enabled), "\t\t     ", format(round(ms$fur_compression_frac, 2), nsmall = 2), "\t", format(round(an$body_density, 2), nsmall = 2), "\t\t", sQuote(ms$user_allometry_enabled), "\t\t", "\n")
  row32 <- c("", "\n")
  row33 <- c("User-Specified Metabolic Options", "\n")
  row34 <- c("User supplied  \t\tAssume activity heat\tdec % variance from  Increasing act hrs: 0=If ACTIV=Y     Minimum forage rate       dec % of energy for      dec % of energy for ", "\n")
  row35 <- c("metabolic rate\t\tcontribs to thrmreg\texpected met rate    1=If QMETAB>QMIN; 2=If act mults\t(Enter as multiple        activity released as     production released as  ", "\n")
  row36 <- c("(Y/N)      Met rate (W)     (Y/N)\t\tto trigger thrmreg    balance; 3=Other min forage rate\t of basal metabolism)     heat that can affect Tc  heat that can affect Tc   ", "\n")
  row37 <- c("----       ----------   -------------------      ----------------    --------------------------------\t---------------------      -----------------------   -------------------", "\n")
  row38 <- c(sQuote(an$user_metabolic_rate_enabled), "\t     ", format(round(an$metabolic_rate, 2), nsmall = 2), "\t\t", sQuote(ms$activity_heat_enabled), "\t\t\t", format(round(ms$thermoreg_trigger_tolerance, 2), nsmall = 2), "\t\t\t", ms$activity_hours_method, "\t\t\t\t", format(round(ms$forage_rate_min, 2), nsmall = 2), "\t\t\t", format(round(ms$activity_heat_fraction, 2), nsmall = 2), "\t\t\t", format(round(ms$production_heat_fraction, 2), nsmall = 2), "\n")
  row39 <- c("", "\n")
  row40 <- c("Fur Properties - LEGS   Note to user *** use same values for all if modeling as a single lump\t\t\t\t", "\n")
  row41 <- c("Hair dia  Hair length   Fur depth   Hair dens. fur        fur       fur      \t\t\t\t", "\n")
  row42 <- c("(um)      (mm)         (mm)         (1/cm2)    front_refl back_refl tran  \t\t\t\t", "\n")
  row43 <- c("dsl vntl  dsl  vntl     dsl vnt      dsl vntl  nd         nd        nd  \t\t\t\t", "\n")
  row44 <- c("----  ---  ---- -----   --- ---      ---- ----   ----    ----      ---- \t\t\t\t", "\n")
  row45 <- .fur_row_leg(fur_leg)
  row46 <- c("\t\t\t\t", "\n")
  row47 <- c("Fur Properties - HEAD & NECK           \t\t\t\t", "\n")
  row48 <- c("Hair dia  Hair length   Fur depth   Hair dens. fur        fur          fur     \t\t\t\t", "\n")
  row49 <- c("(um)      (mm)         (mm)         (1/cm2)    dorsl_refl ventrl_refl  tran   ", "\n")
  row50 <- c("dsl vntl  dsl  vntl     dsl vnt      dsl vntl   nd         nd          nd    nd means NOT %, but decimal ratio! Max. value = 1.00", "\n")
  row51 <- c("---- ---  ---- -----   ---  ---    ----  ----   ----      -------     ----   ", "\n")
  row52 <- .fur_row_other(fur_hn)
  row53 <- c("", "\n")
  row54 <- c("Fur Properties - TORSO           ", "\n")
  row55 <- c("Hair dia  Hair length   Fur depth   Hair dens.  fur         fur         fur      ", "\n")
  row56 <- c("(um)      (mm)         (mm)         (1/cm2)     dorsl_refl ventrl_refl tran   ", "\n")
  row57 <- c("dsl vntl  dsl  vntl     dsl vnt      dsl vntl   nd         nd          nd      ", "\n")
  row58 <- c("---- ---  ---- -----   --- ---      ---- ----   ----       -----       ----    ", "\n")
  row59 <- .fur_row_other(fur_trs)
  row60 <- c("", "\n")
  row61 <- c("Fur Properties - TAIL           ", "\n")
  row62 <- c("Hair dia  Hair length   Fur depth   Hair dens.  fur         fur         fur      ", "\n")
  row63 <- c("(um)      (mm)         (mm)         (1/cm2)     dorsl_refl ventrl_refl tran   ", "\n")
  row64 <- c("dsl vntl  dsl  vntl     dsl vnt      dsl vntl   nd         nd          nd      ", "\n")
  row65 <- c("---- ---  ---- -----   --- ---      ---- ----   ----       -----       ----    ", "\n")
  row66 <- .fur_row_other(fur_tail)
  row67 <- c("", "\n")
  row68 <- c("Hair/Feather length (mm) - dorsal", "\n")
  row69 <- c("This allows for TORSO insulation due to fur/feathers to change seasonally.", "\n")
  row70 <- c("Not SI data.", "\n")
  row71 <- c("------------------------------------------------------------------------------------------------------------- ", "\n")
  row72 <- if (fr$torso_fur_by_julday_enabled == 0) c(paste(rep(fur_trs$hair_length_dorsal, julnum), collapse = " "), "\n") else c(paste(fr$torso_hair_length_dorsal_by_julday, collapse = " "), "\n")
  row73 <- c("", "\n")
  row74 <- c("Hair/Feather length (mm) - ventral", "\n")
  row75 <- c("This allows for TORSO insulation due to fur/feathers to change seasonally.", "\n")
  row76 <- c("Not SI data.", "\n")
  row77 <- c("------------------------------------------------------------------------------------------------------------- ", "\n")
  row78 <- if (fr$torso_fur_by_julday_enabled == 0) c(paste(rep(fur_trs$hair_length_ventral, julnum), collapse = " "), "\n") else c(paste(fr$torso_hair_length_ventral_by_julday, collapse = " "), "\n")
  row79 <- c("", "\n")
  row80 <- c("Pelt/plumage depth (mm)- dorsal", "\n")
  row81 <- c("This allows for TORSO insulation due to fur/feathers to change seasonally.", "\n")
  row82 <- c("Not SI data.", "\n")
  row83 <- c("------------------------------------------------------------------------------------------------------------- ", "\n")
  row84 <- if (fr$torso_fur_by_julday_enabled == 0) c(paste(rep(fur_trs$fur_depth_dorsal, julnum), collapse = " "), "\n") else c(paste(fr$torso_fur_depth_dorsal_by_julday, collapse = " "), "\n")
  row85 <- c("", "\n")
  row86 <- c("Pelt/plumage depth (mm)- ventral", "\n")
  row87 <- c("This allows for TORSO insulation due to fur/feathers to change seasonally.", "\n")
  row88 <- c("Not SI data.", "\n")
  row89 <- c("------------------------------------------------------------------------------------------------------------- ", "\n")
  row90 <- if (fr$torso_fur_by_julday_enabled == 0) c(paste(rep(fur_trs$fur_depth_ventral, julnum), collapse = " "), "\n") else c(paste(fr$torso_fur_depth_ventral_by_julday, collapse = " "), "\n")
  row91 <- c(" ", "\n")
  row92 <- c("PHYSIOLOGICAL properties - temperature and water loss from metabolism & skin\t\t\t\t\t\tFlesh thermal                       User supplied  fur           Depth in fur for radiant exchange", "\n")
  row93 <- c("Core     Core   Core    Min. diff   Texpir-     % skin wet  Max % skin    Sweat OK?\tPiloerect?  Max pilo     conductivity (0.412 - 2.8 W/mC)    thermal conductivity (W/mC)? 1.0= fur surface;", "\n")
  row94 <- c("regul_T  max T  min T   Tc-Tskin(C) Tair (C)    (sweat)     wet (sweat)   (Y/N)\t\t(Y/N)\t    Pct (%)      Start    Minimum  Maximum           (Y/N)    Value               0.5=halfway b/w fur and skin", "\n")
  row95 <- c("----     -----  ----   ----------   -------     ----------  -----------   ---------    ----------   --------\t -------  -------  -------\t      -----    -----               ---------------", "\n")
  row96 <- c(format(round(ph$core_temp_target, 1), nsmall = 1), "\t", format(round(ph$core_temp_max, 1), nsmall = 1), "\t", format(round(ph$core_temp_min, 1), nsmall = 1), "\t   ", format(round(ph$core_skin_temp_diff_min, 1), nsmall = 1), "\t     ", format(round(ph$exhaled_air_temp_offset, 1), nsmall = 1),
             "\t ", format(round(ph$skin_wetness_pct, 1), nsmall = 1), "\t\t", format(round(ph$skin_wetness_max_pct, 1), nsmall = 1), "\t    ", sQuote(ph$sweating_enabled), "\t\t", sQuote(ph$piloerection_enabled), "\t    ", format(round(ph$piloerection_max_pct, 1), nsmall = 1), "\t  ", format(round(ph$flesh_conductivity, 1), nsmall = 1), "\t    ", format(round(ph$flesh_conductivity_min, 1), nsmall = 1),
             "\t    ", format(round(ph$flesh_conductivity_max, 1), nsmall = 1), "\t\t\t", sQuote(ph$user_fur_conductivity_enabled), "\t", format(round(ph$user_fur_conductivity, 1), nsmall = 1), "\t\t\t", format(round(ph$radiant_exchange_fur_depth_frac, 1), nsmall = 1), "\n")
  row97  <- c("", "\n")
  row98  <- c("PHYSIOLOGICAL properties - lungs and gut:  dig. eff.", "\n")
  row99  <- c("O2 extraction      O2 extraction     Gut passage  Fecal water  Urea in", "\n")
  row100 <- c("efficiency max(%)  efficiency min(%) time (days)  (dec. %)     urine (dec. %)", "\n")
  row101 <- c("------------       ---------------- ----------   ----------   --------------", "\n")
  row102 <- c(format(round(ph$o2_extraction_max_pct, 1), nsmall = 1), "\t\t\t", format(round(ph$o2_extraction_min_pct, 1), nsmall = 1), "\t     ", format(round(di$gut_passage_time_days, 2), nsmall = 2), "\t     ", format(round(di$fecal_water_frac, 2), nsmall = 2), "\t     ", format(round(di$urine_urea_frac, 2), nsmall = 2), "\n")
  row103 <- c("", "\n")
  row104 <- c("PHYSIOLOGICAL properties - monthly values: Core temperature regulated this month", "\n")
  row105 <- c("This can be used to simulate hibernation or other unusual activity timing.", "\n")
  row106 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row107 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row108 <- if (ph$core_temp_by_julday_enabled == 0) c(paste(rep(ph$core_temp_target, julnum), collapse = " "), "\n") else c(paste(ph$core_temp_target_by_julday, collapse = " "), "\n")
  row109 <- c("", "\n")
  row110 <- c("Physiological properties - Digestive efficiencies - monthly values:", "\n")
  row111 <- c("(decimal %)    \"", "\n")
  row112 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row113 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row114 <- c(paste(di$digestive_efficiency, collapse = " "), "\n")
  row115 <- c("", "\n")
  row116 <- c("PHYSIOLOGICAL properties - monthly values: Times basal for activity energy & food for it (1-7)", "\n")
  row117 <- c("This can be used to simulate phenology of food available and reproduction timing.", "\n")
  row118 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row119 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row120 <- c(paste(di$activity_basal_multiple, collapse = " "), "\n")
  row121 <- c("", "\n")
  row122 <- c("PHYSIOLOGICAL properties - monthly values: Times basal for disc. nrg food intake (0-7)", "\n")
  row123 <- c("Can use to simulate phenology of food available and reproduction timing.0=no reprod effort", "\n")
  row124 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row125 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row126 <- c(paste(di$reproduction_basal_multiple, collapse = " "), "\n")
  row127 <- c("", "\n")
  row128 <- c("FOOD properties -  % protein (decimal %): monthly values:", "\n")
  row129 <- c(" 100%=1.00 Nectar except breeding months, then same as creeper (April-May): \"", "\n")
  row130 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row131 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row132 <- c(paste(di$food_protein_frac, collapse = " "), "\n")
  row133 <- c("", "\n")
  row134 <- c("FOOD properties -  % fat: monthly values", "\n")
  row135 <- c("(decimal:  100% is 1.00)", "\n")
  row136 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row137 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row138 <- c(paste(di$food_fat_frac, collapse = " "), "\n")
  row139 <- c("", "\n")
  row140 <- c("FOOD properties - % carbohydrate: monthly values", "\n")
  row141 <- c("(decimal format) 100% carbohydrate = 1.00 decimal", "\n")
  row142 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row143 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row144 <- c(paste(di$food_carb_frac, collapse = " "), "\n")
  row145 <- c("", "\n")
  row146 <- c("FOOD properties - monthly values: % dry matter", "\n")
  row147 <- c("(decimal)(0.25 green veg.;0.75 seed humid stor.; 0.9219 dry seed)", "\n")
  row148 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row149 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row150 <- c(paste(di$food_dry_matter_frac, collapse = " "), "\n")
  row151 <- c("", "\n")
  row152 <- c("BEHAVIORAL properties - monthly values: Diurnal?  ALL BEHAVIOR OPTIONS MUST BE IN CAPS!!!", "\n")
  row153 <- c("(Y/N) (Diurnal value for Diurnal in BEHAV.DATA should be the January value here)", "\n")
  row154 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row155 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row156 <- c(paste(sQuote(di$diurnal_enabled), collapse = " "), "\n")
  row157 <- c("", "\n")
  row158 <- c("BEHAVIORAL properties - monthly values: Nocturnal?", "\n")
  row159 <- c("(Y/N) (Nocturnal value for Nocturnal in BEHAV.DATA should be the January value here)", "\n")
  row160 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row161 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row162 <- c(paste(sQuote(di$nocturnal_enabled), collapse = " "), "\n")
  row163 <- c("", "\n")
  row164 <- c("BEHAVIORAL properties - monthly values: Crepuscular?", "\n")
  row165 <- c("(Y/N)", "\n")
  row166 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row167 <- c("-----------------------------------------------------------------------------------------------------------------------------------", "\n")
  row168 <- c(paste(sQuote(di$crepuscular_enabled), collapse = " "), "\n")
  row169 <- c("", "\n")
  row170 <- c("BEHAVIORAL properties - monthly values: Hibernate?", "\n")
  row171 <- c("(Y/N)", "\n")
  row172 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row173 <- c(" --------------------------------------------------------------------------", "\n")
  row174 <- c(paste(sQuote(di$hibernate_enabled), collapse = " "), "\n")
  row175 <- c("", "\n")
  row176 <- c("Fraction of day hibernating,", "\n")
  row177 <- c("if hibernating", "\n")
  row178 <- c("(0.0 - 1.0)", "\n")
  row179 <- c("--------------------------------------------------------------------------", "\n")
  row180 <- c(paste(di$hibernation_day_frac, collapse = " "), "\n")
  row181 <- c("", "\n")
  row182 <- c("BEHAVIORAL properties - monthly values:\t\t\t\t\t\t****NOTE: use 'W' to model an animal floating on the water. To model a fully submerged animal", "\n")
  row183 <- c("Active on Land (L) or Water (W) for each simulation day?\t\t\t  that spends some time on land and some time in the water, use 'L' here and then use", "\n")
  row184 <- c(paste(ms$juldays, collapse = ". "), "\t\t\t  the dive option and dive table to model the time in the water", "\n")
  row185 <- c("--------------------------------------------------------", "\n")
  row186 <- c(paste(di$active_land_or_water, collapse = " "), "\n")
  row187 <- c("", "\n")
  row188 <- c("BEHAVIORAL properties - monthly values:", "\n")
  row189 <- c("Inactive on Land (L) or Water (W) for each simulation day(?)", "\n")
  row190 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row191 <- c("--------------------------------------------------------", "\n")
  row192 <- c(paste(di$inactive_land_or_water, collapse = " "), "\n")
  row193 <- c("", "\n")
  row194 <- c("Time dependent changes in mass (kg)", "\n")
  row195 <- c("This allows for seasonal changes to to fat loss or gain or growth", "\n")
  row196 <- c(paste(ms$juldays, collapse = ". "), "\n")
  row197 <- c("--------------------------------------------------------", "\n")
  row198 <- if (an$mass_by_julday_enabled == 0) c(paste(rep(an$body_mass, julnum), collapse = " "), "\n") else c(paste(an$mass_by_julday, collapse = " "), "\n")
  row199 <- c("", "\n")
  row200 <- c("% body fat composition", "\n")
  row201 <- c("This allows for insulation due to fat to change seasonally.", "\n")
  row202 <- c("Whether it is subcutaneous or body fat is determined by the user in the 3rd data line, 3rd data element above.", "\n")
  row203 <- c("-------------------------------------------------------------------------------------------------------------", "\n")
  row204 <- if (an$body_fat_pct_by_julday_enabled == 0) c(paste(rep(an$body_fat_pct, julnum), collapse = " "), "\n") else c(paste(an$body_fat_pct_by_julday, collapse = " "), "\n")
  row205 <- c("", "\n")
  row206 <- c("Hibernation body temperature (if hibernate), else set to lowest body temperature.", "\n")
  row207 <- c("This allows for seasonal changes in body temperature.", "\n")
  row208 <- c("Whether it is subcutaneous or body fat is determined by the user in the 3rd data line, 3rd data element above.", "\n")
  row209 <- c("-------------------------------------------------------------------------------------------------------------", "\n")
  row210 <- c(paste(rep(ph$hibernation_core_temp, julnum), collapse = " "), "\n")
  row211 <- c("", "\n")
  row212 <- c("BEHAVIORAL: Use nest Climb    Ground shade Dive      Seek Wind\t Night shade?      Dive?\tActive in\t      TR order:\t Burrow/Nest TR use option:", "\n")
  row213 <- c("Burrow OK?  for TR?  to cool? seeking OK?  to cool?  Protection? (Cold protection) Dive option\tshade in day?\t      Behav 1st?   1= both hot & cold", "\n")
  row214 <- c("(Y/N)       (Y/N)    (Y/N)    (Y/N)        (Y/N)     (Y/N)\t (Y/N)            (Y/N)\t\t(Y/N); (S)tand/(L)ie\t(Y/N)\t   2= only hot 3= only cold\tWade (Y/N)?", "\n")
  row215 <- c("-------    -------   -----    ----------   --------  -----------  -------------    ---------\t-----/-------\t      ---------\t-------------------------\t----------", "\n")
  row216 <- c(sQuote(tr$burrow_enabled), "\t    ", sQuote(tr$nest_thermoreg_enabled), "\t     ", sQuote(tr$climb_enabled), "\t  ", sQuote(tr$shade_seeking_enabled), "\t   ", sQuote(tr$dive_cooling_enabled), "\t\t", sQuote(tr$wind_seeking_enabled), "\t\t", sQuote(tr$night_shade_enabled), "\t\t", sQuote(tr$dive_option_enabled), "\t", sQuote(tr$active_in_shade_enabled), "\t", sQuote(tr$shade_posture), "\t\t  ", sQuote(tr$behavior_first_enabled),
              "\t\t", tr$burrow_nest_use_option, "\t\t\t", sQuote(tr$wade_enabled), "\n")
  row217 <- c("", "\n")
  row218 <- c("THRMREG:    Tree sleep  Huddle    # animals  Huddled      Huddled    Concurrent  Tc increase  Tc trigger  O2 extref  SkinW       Concurrent     Tc decrease   Piloerect   ", "\n")
  row219 <- c("Tree sleep?  shade     at night? huddled?  drsl contct vntrl contct  Tc increase?  increment\twtr loss   increment   increment  Tc decrease?   increment      increment", "\n")
  row220 <- c("(Y/N)       (%)        (Y/N)     (intgr)    (dec. %)    (dec. %)       (Y/N)       (deg. C)\t(deg. C)    (%)        (%)          (Y/N)         (deg. C)      (dec. %)", "\n")
  row221 <- c("-------     -------     -----    ---------  --------    --------     ----------    ---------     ------    -------    --------     ----------     ----------    --------", "\n")
  row222 <- c(sQuote(tr$tree_sleep_enabled), "\t    ", format(round(tr$tree_sleep_shade_pct, 1), nsmall = 1), "\t ", sQuote(tr$huddle_enabled), "\t   ", tr$huddle_group_size, "\t    ", format(round(tr$huddle_contact_dorsal_frac, 2), nsmall = 2),
              "\t ", format(round(tr$huddle_contact_ventral_frac, 2), nsmall = 2), "\t\t", sQuote(tr$concurrent_core_temp_increase_enabled), "\t    ", format(round(tr$core_temp_change_increment, 2), nsmall = 2), "\t  ", format(round(tr$core_temp_water_loss_trigger, 2), nsmall = 2), "\t    ", format(round(tr$o2_extraction_increment_pct, 1), nsmall = 1),
              "\t      ", format(round(tr$skin_wetness_increment_pct, 1), nsmall = 1), "\t\t", sQuote(tr$concurrent_core_temp_decrease_enabled), "\t    ", format(round(tr$core_temp_change_increment, 2), nsmall = 2), "\t   ", format(round(tr$piloerection_increment_frac, 2), nsmall = 2), "\n")
  row223 <- c("", "\n")
  row224 <- c("BEHAVIORAL  Flight variables             Flight  If flight=yes, If animal is pollinating", "\n")
  row225 <- c("Flight OK? If flight = yes, then specify Velocity  insect, average flight load (g) may be", "\n")
  row226 <- c("(Y/N)      flight metab (W) specified to (m/s)  correct for flight metabolism", "\n")
  row227 <- c("-------    -----------------------------   ---   ----------------------------------", "\n")
  row228 <- c(sQuote(fd$flight_enabled), "\t\t\t", format(round(fd$flight_metabolic_rate, 2), nsmall = 2), "\t\t  ", format(round(fd$flight_velocity, 2), nsmall = 2), "\t\t    ", format(round(fd$flight_load, 2), nsmall = 2), "\n")
  row229 <- c("", "\n")
  row230 <- c("Fossorial(Below    Digger\tIf fossorial,       Arboreal \t  Shelter/Nest type: CYLN=HOLLOW FULL CYL;    NEST/SHELTER USE WHEN", "\n")
  row231 <- c("grd EXCLUSIVELY)?  Dig burrows? OK node#s=2-10;  exclusively?  HFCL=HOLLOW HALF CYLINDER; SPHR=HOLLOW SPHERE  INACTIVE (NOT HIBERNATING)?", "\n")
  row232 <- c("  (Y/N)           (Y/N)\t\telse use 1\t    (Y/N)         FLAT,CUPP,CYLN,HFCL,SPHR,DOME,NONE)            (Y/N)", "\n")
  row233 <- c("----------       ----------\t-------------    --------\t  -------------------------------------           --------", "\n")
  row234 <- c(sQuote(fd$fossorial_enabled), "\t\t  ", sQuote(fd$digging_enabled), "\t\t  ", fd$fossorial_node, "\t\t   ", sQuote(fd$arboreal_enabled), "\t\t\t", sQuote(ns$shelter_type), "\t\t\t\t\t ", sQuote(ns$nest_use_when_inactive_enabled), "\n")
  row235 <- c("", "\n")
  row236 <- c("CONFIGURATION FACTORS FOR DIFFUSE IR    User-supplied", "\n")
  row237 <- c("Fasky   Fagrd  Fabush/near object \tNu-Re Correlation\tFront\t Side", "\n")
  row238 <- c("0-0.5   0-0.5  0-0.5   \t\t\tCoefficients? (Y/N)     a     b       a      b", "\n")
  row239 <- c("-----   -----  -----  \t\t\t-------------------    ----  ----   ----  ----", "\n")
  row240 <- c(format(round(ms$ir_config_factor_sky, 1), nsmall = 1), "\t", format(round(ms$ir_config_factor_ground, 1), nsmall = 1), "\t ", format(round(ms$ir_config_factor_objects, 1), nsmall = 1), "\t\t\t\t", sQuote(ms$user_nu_re_enabled), "\t       ", format(round(ms$nu_re_front_a, 2), nsmall = 2), "  ", format(round(ms$nu_re_front_b, 2), nsmall = 2),
              "   ", format(round(ms$nu_re_side_a, 2), nsmall = 2), "  ", format(round(ms$nu_re_side_b, 2), nsmall = 2), "\n")
  row241 <- c("", "\n")
  row242 <- c("NEST properties: if nest THICKNESS = 0.0, no nest is assumed\"\t   Shelter/nest # animals in nest\t   If nest/rest place above ground = 'A'\t\tIf nest is below ground,'B',then how deep", "\n")
  row243 <- c("Nest wall            Nest wall (wood: 0.10-0.35;sheep wool:0.05)  to adjust # present for xtra\t\t   If nest/rest place below ground = 'B'\t\tis the nest, i.e., what is the node number (2-10)?", "\n")
  row244 <- c("thickness (m)        thermal conductivity (W/m-C)\t           heat production. If <1, no nest calc.   If nest='B',set 'Burrow OK'='N',so stay @ 1 depth\tExample: node 6 is at 20 cm. (see bottom line below)", "\n")
  row245 <- c("---------------      ----------------------------\t    \t --------------------------------------    ------------------------------\t\t\t\t---------------------------------------------", "\n")
  row246 <- c(format(round(ns$nest_wall_thickness, 2), nsmall = 2), "\t\t\t", format(round(ns$nest_wall_conductivity, 2), nsmall = 2), "\t\t\t\t\t\t\t", format(round(ns$nest_occupants, 0), nsmall = 0), "\t\t\t\t\t\t", sQuote(ns$nest_location), "\t\t\t\t\t\t\t", format(round(ns$nest_soil_node, 0), nsmall = 0), "\n")
  row247 <- c("", "\n")
  row248 <- c("AIR (Burrow gas properties; atm values in parens) NOTE:THESE MUST SUM TO 100.0%", "\n")
  row249 <- c(" % O2     %CO2      %N2", "\n")
  row250 <- c("(20.95%)  (0.03%)  (79.02%) = standard atmosphere", "\n")
  row251 <- c("--------  -------  --------", "\n")
  row252 <- c(format(round(fd$burrow_o2_pct, 2), nsmall = 2), "\t   ", format(round(fd$burrow_co2_pct, 2), nsmall = 2), "    ", format(round(fd$burrow_n2_pct, 2), nsmall = 2), "\n")
  row253 <- c("", "\n")
  row254 <- c("SOIL,BURROW properties\t\t\tSegment\t        Burrow", "\n")
  row255 <- c("Soil type; Finesand=1 sandyloam=2 \tlength/day\tdepth", "\n")
  row256 <- c("gravelly sand=3 clay=4\t\t\t(m)\t        (m)", "\n")
  row257 <- c("-----------------------------------\t----------\t------", "\n")
  row258 <- c(fd$soil_type, "\t\t\t\t\t", format(round(fd$burrow_segment_length, 2), nsmall = 2), "\t\t", format(round(fd$burrow_depth, 2), nsmall = 2), "\n")
  row259 <- c("", "\n")
  row260 <- c("Shelter/Nest properties  (Hominid Paleoshelter)\t\t\t     Shelter transient?", "\n")
  row261 <- c("Used when nest type is not 'NONE' and multiplier >= 1          \t     Is there a tree/log or large shelter", "\n")
  row262 <- c("Length(m) Outer diameter(m) Solar reflectivity(decimal: 1.0 = 100%)  that needs to be run as a transient (Y/N)", "\n")
  row263 <- c("--------- ----------------- ------------------------------------     ------------------------------------------", "\n")
  row264 <- c(format(round(ns$shelter_length, 2), nsmall = 2), "\t\t", format(round(ns$shelter_outer_diameter, 2), nsmall = 2), "\t\t\t", format(round(ns$shelter_solar_reflectivity, 2), nsmall = 2), "\t\t\t\t\t\t", sQuote(ns$shelter_transient_enabled), "\n")
  row265 <- c("", "\n")
  row266 <- c("Density\t      Specific\tNest height rel. to ground surf.(m)", "\n")
  row267 <- c("nest material Heat      + = below surface,", "\n")
  row268 <- c("(kg/m3)\t      (J/kg-C)  - = above surface", "\n")
  row269 <- c("--------    -----------\t---------------", "\n")
  row270 <- c(format(round(ns$shelter_material_density, 2), nsmall = 2), "\t     ", format(round(ns$shelter_material_specific_heat, 2), nsmall = 2), "\t      ", ns$shelter_height_rel_ground, "\n")
  row271 <- c("", "\n")
  row272 <- c("TRANSIENT for SHELTER OR ANIMAL(geometry specified above by shelter/nest type)", "\n")
  row273 <- c("Nodes start from the geometrical center of the hollow", "\n")
  row274 <- c("Number of nodes starting at the inner radius of the shelter to the outer surface", "\n")
  row275 <- c("---------------------------------------------------------", "\n")
  row276 <- c(ns$shelter_node_count, "\n")
  row277 <- c("", "\n")
  row278 <- c("Node locations (m) measured from the geometrical center. Last outer node should be the outer surface.", "\n")
  row279 <- c("Radial dimension of first node should be 0 for an animal, larger than the radius of the animal for a shelter transient.", "\n")
  row280 <- c("For example: for a shelter, if number of nodes = 5: Radii might be 0.03 0.035 0.04 0.05 0.10 ***NOTE: Total thickness must be same as nest wall thickness above***", "\n")
  row281 <- c("-----------------------------------------------------", "\n")
  row282 <- c(ns$shelter_node_radii, "\n")
  row283 <- c("", "\n")
  row284 <- c("# These are comments and not read.: logMR = 0.031*T - 2.27\t\t\t\t(Eq. 1)", "\n")
  row285 <- c("# where T = temperature (in  C) and MR is in mlCO2/hr", "\n")
  row286 <- c("", "\n")
  row287 <- c("# My rederivation for O2 instead of CO2 for Tsetse fly assuming protein diet (RQ = 0.8) lnMR = 0.0714*T - 2.561", "\n")
  row288 <- c("# where T = temperature (in C) and MR is in mlO2/g-h", "\n")
  row289 <- c("", "\n")
  row290 <- c("# Eucalyptus properties like those of hickory, oak")

  endo_path <- file.path(output_dir, "endo.dat")
  cat(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10,
      row11, row12, row13, row14, row15, row16, row17, row18, row19, row20,
      row21, row22, row23, row24, row25, row26, row27, row28, row29, row30,
      row31, row32, row33, row34, row35, row36, row37, row38, row39, row40,
      row41, row42, row43, row44, row45, row46, row47, row48, row49, row50,
      row51, row52, row53, row54, row55, row56, row57, row58, row59, row60,
      row61, row62, row63, row64, row65, row66, row67, row68, row69, row70,
      row71, row72, row73, row74, row75, row76, row77, row78, row79, row80,
      row81, row82, row83, row84, row85, row86, row87, row88, row89, row90,
      row91, row92, row93, row94, row95, row96, row97, row98, row99, row100,
      row101, row102, row103, row104, row105, row106, row107, row108, row109, row110,
      row111, row112, row113, row114, row115, row116, row117, row118, row119, row120,
      row121, row122, row123, row124, row125, row126, row127, row128, row129, row130,
      row131, row132, row133, row134, row135, row136, row137, row138, row139, row140,
      row141, row142, row143, row144, row145, row146, row147, row148, row149, row150,
      row151, row152, row153, row154, row155, row156, row157, row158, row159, row160,
      row161, row162, row163, row164, row165, row166, row167, row168, row169, row170,
      row171, row172, row173, row174, row175, row176, row177, row178, row179, row180,
      row181, row182, row183, row184, row185, row186, row187, row188, row189, row190,
      row191, row192, row193, row194, row195, row196, row197, row198, row199, row200,
      row201, row202, row203, row204, row205, row206, row207, row208, row209, row210,
      row211, row212, row213, row214, row215, row216, row217, row218, row219, row220,
      row221, row222, row223, row224, row225, row226, row227, row228, row229, row230,
      row231, row232, row233, row234, row235, row236, row237, row238, row239, row240,
      row241, row242, row243, row244, row245, row246, row247, row248, row249, row250,
      row251, row252, row253, row254, row255, row256, row257, row258, row259, row260,
      row261, row262, row263, row264, row265, row266, row267, row268, row269, row270,
      row271, row272, row273, row274, row275, row276, row277, row278, row279, row280,
      row281, row282, row283, row284, row285, row286, row287, row288, row289, row290,
      file = endo_path, sep = "")

  # --- Begin building the alomvars.dat file -----------------------------------
  hd  <- al$parts$head
  nk  <- al$parts$neck
  trs <- al$parts$torso
  fl  <- al$parts$front_leg
  rl  <- al$parts$rear_leg
  tl  <- al$parts$tail

  .part_row <- function(p) {
    c(format(round(p$diameter_vertical, 2), nsmall = 2), "\t\t\t", format(round(p$diameter_horizontal, 2), nsmall = 2), "\t\t ", format(round(p$length, 2), nsmall = 2), "    ", format(round(p$fur_depth_dorsal_reference, 1), nsmall = 1), "     ", format(round(p$fur_depth_dorsal_adjusted, 1), nsmall = 1), "\t\t",
      format(round(p$fur_depth_ventral_reference, 1), nsmall = 1), "    ", format(round(p$fur_depth_ventral_adjusted, 1), nsmall = 1), "\t", format(round(p$density, 1), nsmall = 1), "\t\t", format(round(p$geometry, 0), nsmall = 0))
  }

  rowa1  <- c(paste("Animal species = ", an$species_label, sep = ""), "\n")
  rowa2  <- c("Allometry input from taxidermy specimen & sedated individual", "\n")
  rowa3  <- c("Assume head long axis in horizontal plane; dorsal = up = vertical: ventral = belly/bottom  ALL PHOTOGRAPH length units in CM **********.", "\n")
  rowa4  <- c("", "\n")
  rowa5  <- c("Vertebrate/invertebrate group                            Locomotion type", "\n")
  rowa6  <- c("'mammal','birdie','reptil','amphib','insect','btrfly'    bipedal/quadped", "\n")
  rowa7  <- c("----------------------------------------------------     ---------------", "\n")
  rowa8  <- c(sQuote(al$taxon_group), "\t\t\t\t\t\t", sQuote(al$locomotion), "\n")
  rowa9  <- c("", "\n")
  rowa10 <- c("HEAD  Geometry types allowed: cylinder(1),sphere(2),ellipsoidal cyl.(3), ellipsoid (4), truncated cone(5). For cone the vert diam entry is large base diam. Horiz diam entry is the \"snout\" diameter.", "\n")
  rowa11 <- c("Dia.vertical(distal)  Dia.horiz(proximal)  Length  Fur depth(mm)-Midorsl  Midventral       Density(kg/m^3)         Geometry ", "\n")
  rowa12 <- c("----------------      -------------------  ------  --------//---------     -----//-----   --------------       ---------", "\n")
  rowa13 <- c(format(round(hd$diameter_vertical, 2), nsmall = 2), "\t\t\t\t", format(round(hd$diameter_horizontal, 2), nsmall = 2), "\t   ", format(round(hd$length, 2), nsmall = 2), "    ", format(round(hd$fur_depth_dorsal_reference, 1), nsmall = 1), "    ", format(round(hd$fur_depth_dorsal_adjusted, 1), nsmall = 1), "\t  ",
              format(round(hd$fur_depth_ventral_reference, 1), nsmall = 1), "    ", format(round(hd$fur_depth_ventral_adjusted, 1), nsmall = 1), "\t\t", format(round(hd$density, 1), nsmall = 1), "\t\t  ", format(round(hd$geometry, 0), nsmall = 0), "\n")
  rowa14 <- c("", "\n")
  rowa15 <- c("NECK long axis in horizontal plane: same definitions as above: USER -ONLY 2 CHOICES FOR ALL other body parts\t\t\t****NOTE for the fur depths: the first entry is the \"reference\" fur depth that the photo allometry and flesh", "\n")
  rowa16 <- c("Dia.vertical  Dia. horizont  Length  Fur depth(mm)-Midorsl Fur-vntrl(mm) Density(kg/m^3) Geometry cyl(1), elips cyl(3)      dimensions will be based on. The second entry is for users who want to change fur depth without changing", "\n")
  rowa17 <- c("------------  -------------  ------  ------//---------  \t----//----    --------------  ---------\t\t\t\t\t\t\tbody dimensions.**********", "\n")
  rowa18 <- c(format(round(nk$diameter_vertical, 2), nsmall = 2), "\t\t", format(round(nk$diameter_horizontal, 2), nsmall = 2), "\t    ", format(round(nk$length, 2), nsmall = 2), "    ", format(round(nk$fur_depth_dorsal_reference, 1), nsmall = 1), "   ", format(round(nk$fur_depth_dorsal_adjusted, 1), nsmall = 1), "\t\t",
              format(round(nk$fur_depth_ventral_reference, 1), nsmall = 1), "   ", format(round(nk$fur_depth_ventral_adjusted, 1), nsmall = 1), "\t    ", format(round(nk$density, 1), nsmall = 1), "\t   ", format(round(nk$geometry, 0), nsmall = 0), "\n")
  rowa19 <- c("", "\n")
  rowa20 <- c("TORSO: assume long axis in horizontal plane: same definitions as above", "\n")
  rowa21 <- c("Dia.vertical  Dia. horizont  Length  Fur depth(mm)-drsl    \tFur-vntrl(mm)  Density(kg/m^3) Geometry cyl(1), elips cyl(3)", "\n")
  rowa22 <- c("------------  -------------  ------  --------//---------        ----//------    --------------  ---------", "\n")
  rowa23 <- c(format(round(trs$diameter_vertical, 2), nsmall = 2), "\t\t", format(round(trs$diameter_horizontal, 2), nsmall = 2), "\t     ", format(round(trs$length, 2), nsmall = 2), "    ", format(round(trs$fur_depth_dorsal_reference, 1), nsmall = 1), "    ", format(round(trs$fur_depth_dorsal_adjusted, 1), nsmall = 1), "\t\t",
              format(round(trs$fur_depth_ventral_reference, 1), nsmall = 1), "    ", format(round(trs$fur_depth_ventral_adjusted, 1), nsmall = 1), "\t    ", format(round(trs$density, 1), nsmall = 1), "\t\t", format(round(trs$geometry, 0), nsmall = 0), "\n")
  rowa24 <- c("", "\n")
  rowa25 <- c("FRONT LEGS: diameters = sideways, front-back                   If 'birdie', set front leg Geometry = 0.", "\n")
  rowa26 <- c("Dia.sideways  Dia. front-back  Length  Fur depth(mm)-drsl  Fur-Midventral Density(kg/m^3) Geometry cyl(1), ellips cyl(3)", "\n")
  rowa27 <- c("------------  -------------    ------  -------//----------  -----//----- -------------  ---------", "\n")
  rowa28 <- c(format(round(fl$diameter_vertical, 2), nsmall = 2), "\t\t", format(round(fl$diameter_horizontal, 2), nsmall = 2), "\t\t", format(round(fl$length, 2), nsmall = 2), "    ", format(round(fl$fur_depth_dorsal_reference, 1), nsmall = 1), "   ", format(round(fl$fur_depth_dorsal_adjusted, 1), nsmall = 1), "\t  ",
              format(round(fl$fur_depth_ventral_reference, 1), nsmall = 1), "    ", format(round(fl$fur_depth_ventral_adjusted, 1), nsmall = 1), "\t  ", format(round(fl$density, 1), nsmall = 1), "\t   ", format(round(fl$geometry, 0), nsmall = 0), "\n")
  rowa29 <- c("", "\n")
  rowa30 <- c("BACK LEGS: diameters = sideways, front-back", "\n")
  rowa31 <- c("Dia.sideways  Dia. front-back  Length  Fur depth(mm)-drsl  Fur-Midventral Density(kg/m^3) Geometry cyl(1), ellips cyl(3)", "\n")
  rowa32 <- c("------------  -------------    ------  --------//---------  ----//------ --------------  ---------", "\n")
  rowa33 <- c(format(round(rl$diameter_vertical, 2), nsmall = 2), "\t\t", format(round(rl$diameter_horizontal, 2), nsmall = 2), "\t\t", format(round(rl$length, 2), nsmall = 2), "    ", format(round(rl$fur_depth_dorsal_reference, 1), nsmall = 1), "    ", format(round(rl$fur_depth_dorsal_adjusted, 1), nsmall = 1), "\t  ",
              format(round(rl$fur_depth_ventral_reference, 1), nsmall = 1), "    ", format(round(rl$fur_depth_ventral_adjusted, 1), nsmall = 1), "\t  ", format(round(rl$density, 1), nsmall = 1), "\t   ", format(round(rl$geometry, 0), nsmall = 0), "\n")
  rowa34 <- c("", "\n")
  rowa35 <- c("TAIL/ADDITIONAL APPENDAGE: assume long axis in horizontal plane: same definitions as with torso above   Geom: cyl(1),elips      Is 6th part a tail   **Use either if not modeling", "\n")
  rowa36 <- c("Dia.vert (proximal)  Dia. horiz (distal)  Length  Fur depth(mm)-drsl    Fur-vntrl(mm)  Density(kg/m^3)  cyl(3),trunc cone(5)    (T) or proboscis (P) **a 6th appendage", "\n")
  rowa37 <- c("------------        -------------         ------  --------//---------  -----//-----     --------------  ---------               --------------------", "\n")
  rowa38 <- c(format(round(tl$diameter_vertical, 2), nsmall = 2), "\t\t\t", format(round(tl$diameter_horizontal, 2), nsmall = 2), "\t\t ", format(round(tl$length, 2), nsmall = 2), "    ", format(round(tl$fur_depth_dorsal_reference, 1), nsmall = 1), "     ", format(round(tl$fur_depth_dorsal_adjusted, 1), nsmall = 1), "\t\t",
              format(round(tl$fur_depth_ventral_reference, 1), nsmall = 1), "    ", format(round(tl$fur_depth_ventral_adjusted, 1), nsmall = 1), "\t", format(round(tl$density, 1), nsmall = 1), "\t\t", format(round(tl$geometry, 0), nsmall = 0), "\t\t\t", sQuote(al$locomotion), "\n")
  rowa39 <- c("", "\n")
  rowa40 <- c("Absolute measurement (cm)   Shoulder hyt(1);Torso diam (2); Tot length1 (no tail)(3)\tADJUST INITIAL DIMENSIONS", "\n")
  rowa41 <- c("(Real physical dimension)\tTot length2 (incl.tail)(4); Bipedal (5)                      (0=no adjustment; 1=adjust radially; 2= adjust all dimensions) ", "\n")
  rowa42 <- c("------------------  ---------------------------------------------------------   ----------------------------------------------------------------", "\n")
  rowa43 <- c(format(round(al$absolute_measurement_cm, 2), nsmall = 2), "\t\t\t\t\t", format(round(al$absolute_measurement_type, 0), nsmall = 0), "\t\t\t\t\t\t", format(round(al$dimension_adjustment, 0), nsmall = 0), "\n")
  rowa44 <- c("", "\n")
  rowa45 <- c("Subcutaneous Fat on Body Parts (Y/N)\t\t\t\t\t Where does time-dependent mass change come from? ", "\n")
  rowa46 <- c("Head\tNeck\tTorso\tFront Legs\tBack Legs  Tail\t\t 0 = all parts proportionally; 1= torso only (INTEGER!)", "\n")
  rowa47 <- c("----\t----\t-----\t----------\t---------  ----\t\t -------------------------------------------------", "\n")
  rowa48 <- c(sQuote(al$subcutaneous_fat_head_enabled), "\t", sQuote(al$subcutaneous_fat_neck_enabled), "\t", sQuote(al$subcutaneous_fat_torso_enabled), "\t  ", sQuote(al$subcutaneous_fat_front_leg_enabled), "\t\t", sQuote(al$subcutaneous_fat_rear_leg_enabled), "\t   ", sQuote(al$subcutaneous_fat_tail_enabled), "\t\t\t", format(round(al$mass_change_source, 0), nsmall = 0), "\n")
  rowa49 <- c("", "\n")
  rowa50 <- c("Post1  Post2  Post3  Post4     Start Sleep Posture    Start Shade Posture   End Inactive Posture   **Note: Posture 1 = all body parts still modeled, but all in contact with ground", "\n")
  rowa51 <- c("(Y/N)  (Y/N)  (Y/N)  (Y/N)\t\t     (1-4)\t\t\t\t   (1-4)\t\t\t\t(1-4)  Post 2 = legs lumped into torso, head/neck still held up. Post 3 = legs lumped", "\n")
  rowa52 <- c("-----  -----  -----  -----     -------------------    -------------------   --------------------\t\t   into torso, head/neck in contact with ground. Posture 4 = single lump.", "\n")
  rowa53 <- c(sQuote(al$posture_1_enabled), "\t", sQuote(al$posture_2_enabled), "\t", sQuote(al$posture_3_enabled), "\t", sQuote(al$posture_4_enabled), "\t\t", format(round(al$sleep_start_posture, 0), nsmall = 0), "\t\t\t", format(round(al$shade_start_posture, 0), nsmall = 0), "\t\t\t", format(round(al$inactive_end_posture, 0), nsmall = 0), "\n")
  rowa54 <- c("", "\n")
  rowa55 <- c("MinFlshK MinFlshK MinFlshK  MinFlshK  MinFlshK  MinFlshK    Variable core temp? (Y/N)      Torso overhang (for leg shade) - horizontal distance between       Leg vertical offset (for leg shade) - vertical distance      Bird sleep            Bird sleep on  NOTE: these last 2 variables only kick in if it's a bird.  ", "\n")
  rowa56 <- c("Head     Neck\t  Torso\t    Front Leg.Rear Leg  Tail     Legs  6APNDG 6APDNG on ground?    widest point on torso and lateral edge of leg (from front view)      between torso and top of top of variable-core-temp       leg  standing?(Y/N)     1 or 2 legs?", "\n")
  rowa57 <- c("----    -----\t  -----\t   ---------   --------  ------ ----- ------ -----------------   --------------------------------------------------------------     -------------------------------------------------------         ------------      -------------", "\n")
  rowa58 <- c(format(round(al$flesh_conductivity_min_head, 2), nsmall = 2), "\t", format(round(al$flesh_conductivity_min_neck, 2), nsmall = 2), "\t  ", format(round(al$flesh_conductivity_min_torso, 2), nsmall = 2), "\t    ", format(round(al$flesh_conductivity_min_front_leg, 2), nsmall = 2), "\t     ", format(round(al$flesh_conductivity_min_rear_leg, 2), nsmall = 2), "\t", format(round(al$flesh_conductivity_min_tail, 2), nsmall = 2), "\t",
              sQuote(al$variable_core_temp_legs_enabled), "\t", sQuote(al$variable_core_temp_sixth_appendage_enabled), "\t", sQuote(al$sixth_appendage_on_ground_enabled), "\t\t\t", format(round(al$torso_overhang, 2), nsmall = 2), "\t\t\t\t\t\t\t\t", format(round(al$leg_vertical_offset, 2), nsmall = 2), "\t\t\t\t\t\t\t", sQuote(al$bird_sleep_standing_enabled), "\t\t\t", format(round(al$bird_sleep_leg_count, 0), nsmall = 0), "\n")
  rowa59 <- c("", "\n")
  rowa60 <- c("Tc reduced (Y/N)?       Tc=fraction or difference    Fraction (0-1): 1= core,                                        Let leg temp            Let 6th app. temp", "\n")
  rowa61 <- c("Legs     6th Appendage     from local temp (D/F)      0.5 = halfway,  0=ground      Difference     Min Temp (C)    increase if hot? (Y/N)    increase if hot? (Y/N)", "\n")
  rowa62 <- c("------   -------------    ------------//---------      ----------//-----------   ------//------    ------------     ------------------       -------------------", "\n")
  rowa63 <- c(sQuote(al$core_temp_reduction_legs_enabled), "\t\t", sQuote(al$core_temp_reduction_sixth_appendage_enabled), "\t\t", sQuote(al$core_temp_reduction_legs_mode), "\t   ", sQuote(al$core_temp_reduction_sixth_appendage_mode), "\t\t", format(round(al$core_temp_reduction_legs_fraction, 2), nsmall = 2), "\t   ", format(round(al$core_temp_reduction_sixth_appendage_fraction, 2), nsmall = 2), "\t\t",
              format(round(al$core_temp_reduction_legs_difference, 2), nsmall = 2), "\t", format(round(al$core_temp_reduction_sixth_appendage_difference, 2), nsmall = 2), "\t\t", format(round(al$appendage_temp_min, 2), nsmall = 2), "\t\t   ", sQuote(al$leg_temp_increase_if_hot_enabled), "\t\t\t", sQuote(al$sixth_appendage_temp_increase_if_hot_enabled), "\n")
  rowa64 <- c("", "\n")
  rowa65 <- c("**Note: be sure that the minimum and maximum flesh themal conductivity values here do not conflict with endo.dat inputs for overall min/max values.", "\n")
  rowa66 <- c("**Note: be sure to include a minimum flesh thermal K for all body parts regardless of whether they are being modeled.", "\n")

  alomvars_path <- file.path(output_dir, "alomvars.dat")
  cat(rowa1, rowa2, rowa3, rowa4, rowa5, rowa6, rowa7, rowa8, rowa9, rowa10,
      rowa11, rowa12, rowa13, rowa14, rowa15, rowa16, rowa17, rowa18, rowa19, rowa20,
      rowa21, rowa22, rowa23, rowa24, rowa25, rowa26, rowa27, rowa28, rowa29, rowa30,
      rowa31, rowa32, rowa33, rowa34, rowa35, rowa36, rowa37, rowa38, rowa39, rowa40,
      rowa41, rowa42, rowa43, rowa44, rowa45, rowa46, rowa47, rowa48, rowa49, rowa50,
      rowa51, rowa52, rowa53, rowa54, rowa55, rowa56, rowa57, rowa58, rowa59, rowa60,
      rowa61, rowa62, rowa63, rowa64, rowa65, rowa66,
      file = alomvars_path, sep = "")

  log_df <- data.frame(
    file_path = c(endo_path, alomvars_path),
    step      = c("write_endo_dat", "write_alomvars_dat"),
    status    = c("success", "success"),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    stringsAsFactors = FALSE
  )
  if (!is.null(study_area)) log_df$study_area <- study_area

  invisible(log_df)
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): date/time resolution helpers
# --------------------------------------------------------------------------- #

.mtc_resolve_dates <- function(dates, tz = "America/Denver") {
  if (!inherits(dates, "Date") || length(dates) < 1)
    stop("'dates' must be a Date vector (length-2 range or specific dates)")

  all_dates   <- if (length(dates) == 2) seq(dates[1], dates[2], by = "day") else dates
  local_dates <- sort(unique(all_dates))

  if (length(local_dates) > 52)
    stop(sprintf("'dates' resolves to %d unique days; maximum allowed is 52",
                 length(local_dates)))

  midnight_local <- as.POSIXct(paste(local_dates, "00:00:00"), tz = tz)
  utc_start <- lubridate::with_tz(midnight_local, "UTC")
  utc_end   <- utc_start + lubridate::hours(24)

  data.frame(date = local_dates, doy = lubridate::yday(local_dates),
            utc_start = utc_start, utc_end = utc_end)
}

.mtc_match_time_index <- function(time_utc, date_bounds) {
  rows <- vector("list", nrow(date_bounds))
  for (i in seq_len(nrow(date_bounds))) {
    idx <- sort(which(time_utc >= date_bounds$utc_start[i] &
                      time_utc <  date_bounds$utc_end[i]))
    if (length(idx) < 24) {
      warning(sprintf("Only %d/24 hourly records found for %s; skipping this day",
                      length(idx), date_bounds$date[i]))
      next
    }
    idx <- idx[seq_len(24)]
    rows[[i]] <- data.frame(
      date        = date_bounds$date[i],
      doy         = date_bounds$doy[i],
      hour_offset = 0:23,
      utc_idx     = idx,
      utc_time    = time_utc[idx]
    )
  }
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0)
    stop("No requested dates have complete (24-hour) coverage in the source time axis")
  out <- do.call(rbind, rows)
  out[order(out$date, out$hour_offset), ]
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): solar zenith angle
# --------------------------------------------------------------------------- #

.mtc_compute_zen <- function(utc_time, lon, lat, tz = "America/Denver") {
  old_tz <- Sys.getenv("TZ")
  Sys.setenv(TZ = "UTC")
  on.exit(Sys.setenv(TZ = old_tz), add = TRUE)

  utc_time   <- as.POSIXct(utc_time, tz = "UTC")
  local_time <- lubridate::with_tz(utc_time, tz)
  solar_time <- solaR::local2Solar(local_time, lon = lon)

  solar_days <- as.POSIXct(unique(lubridate::date(solar_time)), tz = "UTC")
  solD <- solaR::fSolD(lat = lat, BTd = solar_days, method = "michalsky")
  solI <- solaR::fSolI(solD = solD, BTi = solar_time, sample = "hour",
                       EoT = TRUE, keep.night = TRUE)

  cos_thz <- zoo::coredata(solI$cosThzS)
  zen <- acos(pmin(pmax(cos_thz, -1), 1)) * 180 / pi
  pmin(zen, 90)
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): cell resolution
# --------------------------------------------------------------------------- #

.mtc_grid_template <- function(grid) {
  terra::rast(nrows = grid$nrow, ncols = grid$ncol,
             xmin = grid$xmin, xmax = grid$xmax,
             ymin = grid$ymin, ymax = grid$ymax,
             crs  = grid$crs_wkt)
}

.mtc_resolve_cell <- function(abv_handle, blw_handle, cell, cell_input_type) {
  cell_input_type <- match.arg(cell_input_type, c("index", "lonlat", "cellnumber"))

  if (cell_input_type %in% c("index", "cellnumber")) {
    abv_tmpl <- .mtc_grid_template(abv_handle)
    blw_tmpl <- .mtc_grid_template(blw_handle)
    .check_grid_match(abv_tmpl, blw_tmpl, "abvgrd_input", "blwgrd_input", action = "stop")
    if (abv_handle$nrow != blw_handle$nrow || abv_handle$ncol != blw_handle$ncol)
      stop(sprintf(
        "abvgrd_input and blwgrd_input do not share the same grid dimensions (%d x %d vs %d x %d rows x cols)",
        abv_handle$nrow, abv_handle$ncol, blw_handle$nrow, blw_handle$ncol))
  }

  if (cell_input_type == "index") {
    if (length(cell) != 2)
      stop("'cell' must be length 2 (x_idx, y_idx) for cell_input_type = 'index'")
    x_idx <- as.integer(cell[1]); y_idx <- as.integer(cell[2])
    if (x_idx < 1 || x_idx > abv_handle$ncol || y_idx < 1 || y_idx > abv_handle$nrow)
      stop(sprintf("cell index (%d, %d) is outside the grid (%d cols x %d rows)",
                   x_idx, y_idx, abv_handle$ncol, abv_handle$nrow))
    abv_x_idx <- x_idx; abv_y_idx <- y_idx
    blw_x_idx <- x_idx; blw_y_idx <- y_idx

  } else if (cell_input_type == "cellnumber") {
    if (length(cell) != 1)
      stop("'cell' must be length 1 for cell_input_type = 'cellnumber'")
    cell <- as.integer(cell)
    n_cells <- abv_handle$nrow * abv_handle$ncol
    if (cell < 1 || cell > n_cells)
      stop(sprintf("cell number %d is outside the grid (%d cells)", cell, n_cells))
    abv_y_idx <- ((cell - 1L) %/% abv_handle$ncol) + 1L
    abv_x_idx <- ((cell - 1L) %% abv_handle$ncol) + 1L
    blw_x_idx <- abv_x_idx; blw_y_idx <- abv_y_idx

  } else { # lonlat
    if (length(cell) != 2)
      stop("'cell' must be length 2 (lon, lat) for cell_input_type = 'lonlat'")
    pt <- terra::vect(matrix(cell, nrow = 1), crs = "EPSG:4326")

    abv_tmpl <- .mtc_grid_template(abv_handle)
    xy_abv   <- terra::crds(terra::project(pt, terra::crs(abv_tmpl)))
    abv_x_idx <- terra::colFromX(abv_tmpl, xy_abv[1, 1])
    abv_y_idx <- terra::rowFromY(abv_tmpl, xy_abv[1, 2])
    if (is.na(abv_x_idx) || is.na(abv_y_idx))
      stop("lon/lat falls outside abvgrd_input's grid extent")

    blw_tmpl <- .mtc_grid_template(blw_handle)
    xy_blw   <- terra::crds(terra::project(pt, terra::crs(blw_tmpl)))
    blw_x_idx <- terra::colFromX(blw_tmpl, xy_blw[1, 1])
    blw_y_idx <- terra::rowFromY(blw_tmpl, xy_blw[1, 2])
    if (is.na(blw_x_idx) || is.na(blw_y_idx))
      stop("lon/lat falls outside blwgrd_input's grid extent")
  }

  abv_tmpl <- .mtc_grid_template(abv_handle)
  x_coord  <- terra::xFromCol(abv_tmpl, abv_x_idx)
  y_coord  <- terra::yFromRow(abv_tmpl, abv_y_idx)
  pt_native <- terra::vect(matrix(c(x_coord, y_coord), nrow = 1), crs = terra::crs(abv_tmpl))
  ll <- terra::crds(terra::project(pt_native, "EPSG:4326"))

  list(abv_x_idx = abv_x_idx, abv_y_idx = abv_y_idx,
       blw_x_idx = blw_x_idx, blw_y_idx = blw_y_idx,
       x_coord = x_coord, y_coord = y_coord,
       lon = ll[1, 1], lat = ll[1, 2])
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): NetCDF I/O backend
# --------------------------------------------------------------------------- #

.mtc_open_nc <- function(path) {
  if (!requireNamespace("ncdf4", quietly = TRUE))
    stop('Package \'ncdf4\' is required. Install with: install.packages("ncdf4")')

  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  x_v <- nc$dim$x$vals; y_v <- nc$dim$y$vals
  res_x <- abs(diff(x_v))[1]; res_y <- abs(diff(y_v))[1]

  crs_att <- ncdf4::ncatt_get(nc, "crs", "crs_wkt")
  crs_wkt <- if (isTRUE(crs_att$hasatt)) crs_att$value else NA_character_

  time_units   <- nc$dim$time$units
  origin_str   <- trimws(sub("hours since\\s+", "", time_units))
  origin_str   <- sub("\\s+UTC$", "", origin_str)
  origin_posix <- as.POSIXct(origin_str, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  time_utc     <- origin_posix + nc$dim$time$vals * 3600

  list(kind = "nc", source = path,
       nrow = nc$dim$y$len, ncol = nc$dim$x$len,
       xmin = min(x_v) - res_x / 2, xmax = max(x_v) + res_x / 2,
       ymin = min(y_v) - res_y / 2, ymax = max(y_v) + res_y / 2,
       res_x = res_x, res_y = res_y, crs_wkt = crs_wkt,
       time_utc = time_utc, vars = setdiff(names(nc$var), "crs"))
}

.mtc_read_nc <- function(handle, vars, x_idx, y_idx, time_idx) {
  if (!requireNamespace("ncdf4", quietly = TRUE))
    stop('Package \'ncdf4\' is required. Install with: install.packages("ncdf4")')

  nc <- ncdf4::nc_open(handle$source)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  runs <- split(time_idx, cumsum(c(1, diff(time_idx) != 1)))

  out <- list()
  for (vn in vars) {
    vals <- numeric(length(time_idx))
    pos  <- 1L
    for (run in runs) {
      v <- ncdf4::ncvar_get(nc, vn, start = c(x_idx, y_idx, run[1]),
                            count = c(1, 1, length(run)))
      vals[pos:(pos + length(run) - 1L)] <- v
      pos <- pos + length(run)
    }
    out[[vn]] <- vals
  }
  out
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): HDF5 I/O backend
# --------------------------------------------------------------------------- #

.mtc_open_h5 <- function(path) {
  if (!requireNamespace("rhdf5", quietly = TRUE))
    stop('Package \'rhdf5\' is required. Install with: BiocManager::install("rhdf5")')
  on.exit(rhdf5::H5close(), add = TRUE)

  attrs <- rhdf5::h5readAttributes(path, "/")
  time_str <- rhdf5::h5read(path, "time")
  time_utc <- as.POSIXct(time_str, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  ls <- rhdf5::h5ls(path, recursive = FALSE)
  root_datasets <- ls$name[ls$group == "/" & ls$otype == "H5I_DATASET" & ls$name != "time"]
  blw_groups    <- ls$name[ls$group == "/" & ls$otype == "H5I_GROUP"]
  blw_vars      <- sprintf("Tz_%s", blw_groups)

  list(kind = "h5", source = path,
       nrow = as.integer(attrs$nrow), ncol = as.integer(attrs$ncol),
       xmin = as.numeric(attrs$xmin), xmax = as.numeric(attrs$xmax),
       ymin = as.numeric(attrs$ymin), ymax = as.numeric(attrs$ymax),
       res_x = as.numeric(attrs$res_x), res_y = as.numeric(attrs$res_y),
       crs_wkt = attrs$crs_wkt, time_utc = time_utc,
       vars = c(root_datasets, blw_vars))
}

.mtc_read_h5 <- function(handle, vars, x_idx, y_idx, time_idx) {
  if (!requireNamespace("rhdf5", quietly = TRUE))
    stop('Package \'rhdf5\' is required. Install with: BiocManager::install("rhdf5")')
  on.exit(rhdf5::H5close(), add = TRUE)

  out <- list()
  for (vn in vars) {
    ds_path <- if (grepl("^Tz_BlwGrd_", vn)) sprintf("%s/Tz", sub("^Tz_", "", vn)) else vn
    # write_tile() stores HDF5 datasets in R's native [nrow, ncol, ntime]
    # order (row/y first, then column/x) -- the opposite of the netCDF
    # backend's (x, y, time) dimension order -- so the index list here must
    # be (y_idx, x_idx, time_idx), not (x_idx, y_idx, time_idx).
    v <- rhdf5::h5read(handle$source, ds_path, index = list(y_idx, x_idx, time_idx))
    out[[vn]] <- as.numeric(v)
  }
  out
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): VRT I/O backend
# --------------------------------------------------------------------------- #

.mtc_open_vrt <- function(stem, vrt_files = Sys.glob(sprintf("%s_*.vrt", stem))) {
  if (length(vrt_files) == 0)
    stop(sprintf("No .vrt files found matching stem: %s_*.vrt", stem))

  stem_base <- basename(stem)
  fnames    <- tools::file_path_sans_ext(basename(vrt_files))
  var_names <- substring(fnames, nchar(stem_base) + 2)

  r0  <- terra::rast(vrt_files[1])
  ext <- terra::ext(r0)
  res <- terra::res(r0)

  list(kind = "vrt", source = stem, vrt_files = vrt_files,
       nrow = terra::nrow(r0), ncol = terra::ncol(r0),
       xmin = ext$xmin, xmax = ext$xmax, ymin = ext$ymin, ymax = ext$ymax,
       res_x = res[1], res_y = res[2],
       crs_wkt = terra::crs(r0, proj = FALSE),
       time_utc = as.POSIXct(terra::time(r0), tz = "UTC"),
       vars = var_names)
}

.mtc_read_vrt <- function(handle, vars, x_idx, y_idx, time_idx) {
  out <- list()
  for (vn in vars) {
    vf <- handle$vrt_files[match(vn, handle$vars)]
    r  <- terra::rast(vf)
    cell_no <- terra::cellFromRowCol(r, y_idx, x_idx)
    vals <- terra::extract(r[[time_idx]], cell_no)
    out[[vn]] <- as.numeric(vals[1, ])
  }
  out
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): in-memory SpatRaster I/O backend
# --------------------------------------------------------------------------- #

.mtc_open_spat <- function(r) {
  layer_names <- names(r)
  var_names   <- unique(sub("_[0-9]+$", "", layer_names))

  t_r <- tryCatch(terra::time(r), error = function(e) NULL)
  if (is.null(t_r) || all(is.na(t_r)))
    stop("Input SpatRaster has no time metadata (terra::time(r)); cannot align to requested dates")

  first_var <- var_names[1]
  keep <- sub("_[0-9]+$", "", layer_names) == first_var
  time_utc <- as.POSIXct(t_r[keep], tz = "UTC")

  ext <- terra::ext(r); res <- terra::res(r)
  list(kind = "spat", source = r,
       nrow = terra::nrow(r), ncol = terra::ncol(r),
       xmin = ext$xmin, xmax = ext$xmax, ymin = ext$ymin, ymax = ext$ymax,
       res_x = res[1], res_y = res[2],
       crs_wkt = terra::crs(r, proj = FALSE),
       time_utc = time_utc, vars = var_names, layer_names = layer_names)
}

.mtc_read_spat <- function(handle, vars, x_idx, y_idx, time_idx) {
  r <- handle$source
  cell_no <- terra::cellFromRowCol(r, y_idx, x_idx)
  out <- list()
  for (vn in vars) {
    lyr_idx <- which(sub("_[0-9]+$", "", handle$layer_names) == vn)[time_idx]
    vals <- terra::extract(r[[lyr_idx]], cell_no)
    out[[vn]] <- as.numeric(vals[1, ])
  }
  out
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): unified I/O dispatcher
# --------------------------------------------------------------------------- #

.mtc_open <- function(input) {
  if (inherits(input, "SpatRaster")) return(.mtc_open_spat(input))

  if (!is.character(input) || length(input) != 1)
    stop("Input must be a file path (character) or a SpatRaster")

  ext <- tolower(tools::file_ext(input))
  if (ext == "nc") {
    if (!file.exists(input)) stop(sprintf("File not found: %s", input))
    return(.mtc_open_nc(input))
  }
  if (ext == "h5") {
    if (!file.exists(input)) stop(sprintf("File not found: %s", input))
    return(.mtc_open_h5(input))
  }
  if (ext == "") return(.mtc_open_vrt(input))

  stop(sprintf(
    "Unrecognized input: '%s' (expected .nc, .h5, a .vrt stem, or a SpatRaster)", input))
}

.mtc_read <- function(handle, vars, x_idx, y_idx, time_idx) {
  switch(handle$kind,
        nc   = .mtc_read_nc(handle, vars, x_idx, y_idx, time_idx),
        h5   = .mtc_read_h5(handle, vars, x_idx, y_idx, time_idx),
        vrt  = .mtc_read_vrt(handle, vars, x_idx, y_idx, time_idx),
        spat = .mtc_read_spat(handle, vars, x_idx, y_idx, time_idx),
        stop(sprintf("Unknown handle kind: %s", handle$kind)))
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): ELEV resolution
# --------------------------------------------------------------------------- #

.mtc_resolve_elev <- function(elev, x_coord, y_coord, crs_wkt) {
  if (is.numeric(elev)) {
    if (length(elev) != 1 || is.na(elev))
      stop("'elev' must be a single non-NA numeric value or a DEM path/SpatRaster")
    return(as.numeric(elev))
  }

  dem <- if (inherits(elev, "SpatRaster")) {
    elev
  } else if (is.character(elev) && length(elev) == 1) {
    terra::rast(elev)
  } else {
    stop("'elev' must be numeric, a single file path, or a SpatRaster")
  }

  pt     <- terra::vect(matrix(c(x_coord, y_coord), nrow = 1), crs = crs_wkt)
  pt_dem <- terra::project(pt, terra::crs(dem))
  val    <- terra::extract(dem, pt_dem)[1, 2]

  if (is.na(val))
    stop("'elev' DEM sampling returned NA: the cell falls outside the DEM extent or over a no-data area")

  as.numeric(val)
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): TANNUL resolution
# --------------------------------------------------------------------------- #

.mtc_resolve_tannul <- function(tannul, abv_handle, x_idx, y_idx) {
  if (!is.null(tannul)) {
    if (!is.numeric(tannul) || length(tannul) != 1 || is.na(tannul))
      stop("'tannul' must be a single non-NA numeric value or NULL")
    return(as.numeric(tannul))
  }

  n_hours <- length(abv_handle$time_utc)
  if (n_hours / 24 < 330)
    warning(sprintf(
      "Computing TANNUL from only %.1f days of data (< ~330); this may not represent a full annual cycle",
      n_hours / 24))

  vals <- .mtc_read(abv_handle, "Tz", x_idx, y_idx, seq_len(n_hours))$Tz
  mean(vals, na.rm = TRUE)
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): column-derivation builders
# --------------------------------------------------------------------------- #

.mtc_build_metout <- function(day_index, abv_series, zen, elev, tannul) {
  n <- nrow(day_index)
  sigma <- 5.670374e-8

  taloc <- abv_series$Tz
  rhloc <- abv_series$relhum
  vloc  <- abv_series$windspeed
  solr  <- abv_series$Rdirdown + abv_series$Rdifdown
  tskyc <- (abv_series$Rlwdown / sigma)^0.25 - 273.15

  elev_col <- c(elev, rep(0, n - 1L))

  data.frame(
    DOY    = day_index$doy,
    TIME   = day_index$hour_offset * 60,
    TALOC  = taloc, TAREF = taloc,
    RHLOC  = rhloc, RH    = rhloc,
    VLOC   = vloc,  VREF  = vloc,
    ZEN    = zen,
    SOLR   = solr,
    TSKYC  = tskyc,
    ELEV   = elev_col,
    TANNUL = rep(tannul, n)
  )
}

.mtc_build_soil <- function(day_index, blw_series) {
  depth_vars <- names(blw_series)
  depth_mm   <- as.numeric(sub("^Tz_BlwGrd_", "", depth_vars))
  depth_cm   <- depth_mm / 10
  col_names  <- sprintf("D%scm", format(depth_cm, trim = TRUE, drop0trailing = TRUE))

  ord <- order(depth_cm)
  df  <- data.frame(TIME = day_index$hour_offset * 60)
  for (i in ord) df[[col_names[i]]] <- blw_series[[depth_vars[i]]]
  df
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): value clamping
# --------------------------------------------------------------------------- #

.mtc_clamp_defaults <- function() {
  # SOLR upper bound (1200 W/m^2): defense-in-depth backstop, independent of the
  # upstream microclimfPar radiation fix (di/cos(zenith) beam recovery amplifying
  # near-zero-elevation residuals into 600-1300+ W/m^2 SOLR values -- see
  # HPC_workflow/HANDOFF_endotherm-hang.md). ZEN isn't threaded into this generic,
  # per-column scalar clamp table, so a zenith-aware bound isn't clean here without
  # reworking .mtc_apply_clamp()/the public clamp_bounds schema; a fixed ceiling is
  # used instead. 1200 W/m^2 is comfortably below the flat 1352 (solar constant)
  # ceiling used upstream, and above legitimate clear-sky GHI even at this high-
  # elevation Teton site (typically <=1000-1100 W/m^2 peak), so it only trims
  # genuinely corrupted values, not real weather.
  data.frame(
    variable = c("TALOC", "TAREF", "TANNUL", "RHLOC", "RH", "VLOC", "VREF",
                "ZEN", "SOLR", "TSKYC", "ELEV",
                "D0cm", "D1.5cm", "D2.5cm", "D5cm", "D10cm", "D15cm", "D20cm",
                "D30cm", "D50cm", "D100cm", "D200cm"),
    lower = c(-90, -90, -90, 0, 0, 0, 0,
             0, 0, -100, -500,
             rep(-90, 11)),
    upper = c(60, 60, 60, 100, 100, NA_real_, NA_real_,
             90, 1200, 60, 9000,
             rep(70, 11)),
    stringsAsFactors = FALSE
  )
}

#' Default Clamp Bounds for micro_to_csv()
#'
#' Returns the package's default physically-valid-range table used by
#' \code{\link{micro_to_csv}} to clamp its output columns when
#' \code{clamp = TRUE}.
#'
#' @return A \code{data.frame} with columns \code{variable} (character),
#'   \code{lower}, \code{upper} (numeric, \code{NA} meaning unbounded on
#'   that side). One row per clampable \code{metout}/\code{soil} column
#'   (22 rows). A fresh, independent copy is returned on every call.
#'
#' @details
#' Bounds are set at Earth's physical extremes, not typical values, so
#' clamping only catches genuinely invalid data (e.g. numerical noise
#' pushing \code{SOLR} slightly negative or \code{RH} fractionally over
#' 100) rather than trimming plausible weather. The 11 soil depth rows
#' (\code{D0cm} ... \code{D200cm}) cover both \code{run_micro_big_nichemap}'s
#' depth set (0, 1.5, 5, 10, 15, 20, 30, 50, 100, 200 cm) and NicheMapR's
#' stock 2.5 cm depth, since a \code{blwgrd_input} tile can be built with
#' either. Any \code{soil} depth column not listed here (e.g. from a tile
#' built with a custom depth set) passes through unclamped -- \code{ELEV} is
#' a structural sentinel (elevation on row 1, \code{0} elsewhere per
#' NicheMapR convention, see \code{\link{micro_to_csv}}) rather than a real
#' measurement; patching its \code{lower} bound above \code{0} would corrupt
#' that sentinel. \code{DOY}/\code{TIME} are index columns and are never
#' clamped, so they have no row here. \code{SOLR}'s upper bound (\code{1200}
#' W/m^2) is a defense-in-depth backstop against a since-fixed \code{microclimfPar}
#' radiation bug that could inflate \code{SOLR} into the hundreds to
#' 1000s of W/m^2 near sunrise/sunset; it sits well below the solar constant
#' and above legitimate clear-sky irradiance, so it should never trim real
#' weather.
#'
#' @seealso \code{\link{micro_to_csv}}
#' @export
micro_to_csv_clamp_defaults <- function() {
  .mtc_clamp_defaults()
}

.mtc_apply_clamp <- function(df, bounds) {
  match_cols <- intersect(names(df), bounds$variable)
  for (col in match_cols) {
    b <- bounds[bounds$variable == col, ]
    x <- df[[col]]
    if (!is.na(b$lower)) x <- pmax(x, b$lower)
    if (!is.na(b$upper)) x <- pmin(x, b$upper)
    df[[col]] <- x
  }
  df
}

.mtc_resolve_clamp_bounds <- function(clamp_bounds) {
  defaults <- .mtc_clamp_defaults()

  if (is.null(clamp_bounds)) {
    bounds <- defaults
  } else {
    if (!is.data.frame(clamp_bounds) ||
        !identical(sort(names(clamp_bounds)), sort(c("variable", "lower", "upper"))))
      stop("'clamp_bounds' must be a data.frame with columns 'variable', 'lower', 'upper'")
    is_numeric_or_all_na <- function(x) is.numeric(x) || all(is.na(x))
    if (!is_numeric_or_all_na(clamp_bounds$lower) || !is_numeric_or_all_na(clamp_bounds$upper))
      stop("'clamp_bounds' columns 'lower' and 'upper' must be numeric (NA allowed for unbounded)")
    if (anyNA(clamp_bounds$variable) || anyDuplicated(clamp_bounds$variable))
      stop("'clamp_bounds$variable' must be non-NA and contain no duplicates")

    bounds <- defaults[!defaults$variable %in% clamp_bounds$variable, ]
    bounds <- rbind(bounds, clamp_bounds[, c("variable", "lower", "upper")])
    rownames(bounds) <- NULL
  }

  bad <- !is.na(bounds$lower) & !is.na(bounds$upper) & bounds$lower > bounds$upper
  if (any(bad))
    stop(sprintf("resolved clamp bounds have lower > upper for variable(s): %s",
                paste(bounds$variable[bad], collapse = ", ")))

  bounds
}

# --------------------------------------------------------------------------- #
#  micro_to_csv(): top-level export
# --------------------------------------------------------------------------- #

#' Extract One Grid Cell's Microclimate Time Series as NicheMapR Endotherm Inputs
#'
#' Pulls one cell's time series out of a paired AbvGrd/BlwGrd microclimf tile
#' (as produced by \code{\link{write_tile}}/\code{\link{stitch_tiles}}) and
#' reshapes it into the 4 data frames NicheMapR's Endotherm model expects:
#' \code{metout}, \code{shadmet}, \code{soil}, \code{shadsoil}.
#'
#' @param abvgrd_input Path to an AbvGrd \code{.nc} or \code{.h5} tile file, a
#'   \code{.vrt} stem (no extension or variable suffix -- \code{stitch_tiles}
#'   writes one \code{.vrt} per variable, e.g. \code{"<stem>_Tz.vrt"}), or an
#'   already-open \code{terra::SpatRaster} (as returned by
#'   \code{terra::rast(path)} with no \code{subds}, i.e. layers named
#'   \code{"<variable>_<time_index>"}).
#' @param blwgrd_input Same input kinds as \code{abvgrd_input}, for the
#'   paired BlwGrd tile.
#' @param cell Numeric. The cell to extract, interpreted according to
#'   \code{cell_input_type}: \code{c(x_idx, y_idx)} for \code{"index"},
#'   \code{c(lon, lat)} (WGS84) for \code{"lonlat"}, or a single flat cell
#'   number (row-major) for \code{"cellnumber"}.
#' @param cell_input_type One of \code{"index"}, \code{"lonlat"},
#'   \code{"cellnumber"}.
#' @param dates A length-2 \code{Date} vector (inclusive range) or a vector of
#'   specific \code{Date}s. Resolves to at most 52 unique local calendar days;
#'   \code{stop()}s otherwise. Day component of range endpoints is used as-is
#'   (unlike other package functions, exact days matter here).
#' @param elev Required. Either a single numeric elevation value, or a DEM
#'   file path/\code{SpatRaster} sampled at the cell's coordinates.
#' @param tannul Optional numeric mean annual temperature (Celsius). If
#'   \code{NULL} (default), computed as the mean of \code{Tz} over the full
#'   time axis of \code{abvgrd_input} at this cell.
#' @param tz Optional IANA timezone string for the cell's local clock.
#'   Default \code{"America/Denver"}; override for other sites (e.g.
#'   \code{"America/Anchorage"}).
#' @param clamp Logical. If \code{TRUE}, clamp every column present in the
#'   resolved clamp-bounds table (see \code{clamp_bounds}) to its
#'   \code{[lower, upper]} range in all 4 returned data frames, using
#'   \code{\link{micro_to_csv_clamp_defaults}} unless overridden. Default
#'   \code{FALSE}: output is unchanged from prior behavior.
#' @param clamp_bounds Optional \code{data.frame} with columns
#'   \code{variable}, \code{lower}, \code{upper} (see
#'   \code{\link{micro_to_csv_clamp_defaults}}). Rows here patch the
#'   package defaults: a listed \code{variable} overrides that default's
#'   bounds (or is added if new), any \code{variable} not listed keeps its
#'   default bounds. Ignored when \code{clamp = FALSE}.
#'
#' @return A named list of 4 data frames: \code{metout}, \code{shadmet},
#'   \code{soil}, \code{shadsoil}. No files are written.
#'
#' @details
#' microclimf's tiled output represents one already-vegetation-adjusted
#' microclimate per pixel, not a binary sun/shade pair like classic NicheMapR
#' point models -- so \code{shadmet} is an exact duplicate of \code{metout},
#' and \code{shadsoil} of \code{soil}.
#'
#' \code{TAREF}/\code{RH}/\code{VREF} duplicate \code{TALOC}/\code{RHLOC}/
#' \code{VLOC}: the pipeline only ever runs one model height per point, so
#' there is no separate reference-height (1.2 m) series to source them from.
#' \code{ZEN} is computed via \code{solaR} from the cell's reprojected
#' lon/lat and local solar time, clamped to a maximum of 90 (below-horizon
#' convention). \code{SOLR = Rdirdown + Rdifdown}. \code{TSKYC} is derived
#' from \code{Rlwdown} via Stefan-Boltzmann inversion
#' (\code{(Rlwdown/sigma)^0.25 - 273.15}) -- this is not an approximation:
#' microclimf's \code{Rlwdown} is the same combined downward-longwave
#' quantity NicheMapR's own TSKY formula sums before its final \code{^(1/4)}
#' step. \code{ELEV} follows the NicheMapR output convention of stamping the
#' elevation value on row 1 only and \code{0} for every subsequent row.
#'
#' Source tile files are chunked \code{[nrow,ncol,1]} (nc) or
#' \code{[nrow,ncol,<=24]} (h5). For \code{.nc}/\code{.h5} path inputs, this
#' function reads directly via \code{ncdf4}/\code{rhdf5} start/count
#' indexing rather than through \code{terra}, which is roughly 300x faster
#' for single-cell time series at this chunk size. \code{.vrt} and
#' already-open \code{SpatRaster} inputs go through \code{terra} and may be
#' slower, especially a VRT mosaicking many tiles.
#'
#' @seealso \code{\link{write_tile}}, \code{\link{stitch_tiles}},
#'   \code{\link{micro_to_csv_clamp_defaults}}, and
#'   \code{\link{write_endotherm_inputs}} (writes \code{endo.dat}/
#'   \code{alomvars.dat} -- CSV export of this function's output and running
#'   \code{Endo2022a.exe} are handled by later, separate functions).
#'
#' @export
micro_to_csv <- function(abvgrd_input, blwgrd_input, cell, cell_input_type,
                         dates, elev, tannul = NULL, tz = "America/Denver",
                         clamp = FALSE, clamp_bounds = NULL) {
  cell_input_type <- match.arg(cell_input_type, c("index", "lonlat", "cellnumber"))

  bounds <- if (clamp) .mtc_resolve_clamp_bounds(clamp_bounds) else NULL

  abv_handle <- .mtc_open(abvgrd_input)
  blw_handle <- .mtc_open(blwgrd_input)

  required_abv <- c("Tz", "relhum", "windspeed", "Rdirdown", "Rdifdown", "Rlwdown")
  missing_abv  <- setdiff(required_abv, abv_handle$vars)
  if (length(missing_abv) > 0)
    stop(sprintf("abvgrd_input is missing required variable(s): %s",
                paste(missing_abv, collapse = ", ")))

  blw_depth_vars <- grep("^Tz_BlwGrd_", blw_handle$vars, value = TRUE)
  if (length(blw_depth_vars) == 0)
    stop("blwgrd_input has no Tz_BlwGrd_* depth variables")

  pos <- .mtc_resolve_cell(abv_handle, blw_handle, cell, cell_input_type)

  date_bounds <- .mtc_resolve_dates(dates, tz)

  abv_day_index <- .mtc_match_time_index(abv_handle$time_utc, date_bounds)
  blw_day_index <- .mtc_match_time_index(blw_handle$time_utc, date_bounds)

  if (!identical(abv_day_index[, c("date", "hour_offset")],
                blw_day_index[, c("date", "hour_offset")]))
    stop("abvgrd_input and blwgrd_input do not have matching available hours for the requested dates")

  zen <- .mtc_compute_zen(abv_day_index$utc_time, pos$lon, pos$lat, tz)

  elev_val   <- .mtc_resolve_elev(elev, pos$x_coord, pos$y_coord, abv_handle$crs_wkt)
  tannul_val <- .mtc_resolve_tannul(tannul, abv_handle, pos$abv_x_idx, pos$abv_y_idx)

  abv_series <- .mtc_read(abv_handle, required_abv, pos$abv_x_idx, pos$abv_y_idx,
                          abv_day_index$utc_idx)
  blw_series <- .mtc_read(blw_handle, blw_depth_vars, pos$blw_x_idx, pos$blw_y_idx,
                          blw_day_index$utc_idx)

  metout <- .mtc_build_metout(abv_day_index, abv_series, zen, elev_val, tannul_val)
  soil   <- .mtc_build_soil(blw_day_index, blw_series)

  if (clamp) {
    metout <- .mtc_apply_clamp(metout, bounds)
    soil   <- .mtc_apply_clamp(soil, bounds)
  }

  metout <- .mtc_round3(metout)
  soil   <- .mtc_round3(soil)

  list(metout = metout, shadmet = metout, soil = soil, shadsoil = soil)
}

.mtc_round3 <- function(df) {
  df[] <- lapply(df, function(col) if (is.numeric(col)) round(col, 3) else col)
  df
}

.trapz_proportion <- function(x, y) {
  n <- length(x)
  sum(diff(x) * (y[-n] + y[-1]) / 2)
}

#' Compute mean pelt reflectance from a hand-held spectrometer file
#'
#' Reads a single PSR-3500 spectrometer \code{.sed} output file -- or accepts
#' an already-parsed data.frame of the same shape -- converts its
#' \code{Reflect. \%} column to a 0-1 proportion, and trapezoidally
#' integrates it over wavelength to get a single mean reflectance value --
#' the input expected by \code{\link{write_endotherm_inputs}}'s
#' \code{fur$reflectivity_dorsal}/\code{fur$reflectivity_ventral}
#' (dorsal/ventral pelt reflectance)
#' fields.
#'
#' @param sed_input Either a path (character) to a single \code{.sed}
#'   spectrometer output file, or a \code{data.frame} already containing the
#'   spectral data: a \code{Wvl} column plus a column matching
#'   \code{"Reflect"} (on the same 0-100 percent scale as a \code{.sed}
#'   file's \code{Reflect. \%} column). The \code{data.frame} form lets
#'   callers who already have the data in memory skip file parsing.
#'
#' @return A named list: \code{mean_reflectance} (numeric, 0-1, the
#'   integrated reflectance divided by the data's actual wavelength span),
#'   \code{wvl_min}, \code{wvl_max} (numeric, nm, the actual range of
#'   wavelengths present in the data), \code{n_points} (integer, number of
#'   spectral data rows), and \code{file} (the input \code{sed_input} if it
#'   was a character path, else \code{NA_character_}).
#'
#' @details
#' Ported from \code{RefleCalc_NicheMap.r}. The reflectance column is located
#' by matching \code{"Reflect"} against the table's (\code{make.names()}-
#' mangled, for the file-path input) column headers rather than hardcoding
#' the exact mangled name, so minor header-format differences across
#' instrument software versions -- or a caller's own column naming for the
#' data.frame input -- don't break parsing. Integration uses the trapezoidal
#' rule, implemented directly (numerically equivalent to
#' \code{pracma::trapz(x, y)}) rather than depending on \code{pracma} for a
#' single formula. Unlike the source script, which divided by a hardcoded
#' nominal instrument range of \code{2500 - 350} nm, this function divides by
#' the actual \code{max(Wvl) - min(Wvl)} present in the data, so the result
#' is correct even when the data doesn't span the full nominal range.
#'
#' @seealso \code{\link{write_endotherm_inputs}}
#' @export
compute_pelt_reflectance <- function(sed_input) {
  if (is.character(sed_input) && length(sed_input) != 1)
    stop("'sed_input' must be a single file path or a single data.frame")

  if (is.character(sed_input)) {
    if (!file.exists(sed_input))
      stop(sprintf("'sed_input' does not exist:\n  %s", sed_input))

    lines <- readLines(sed_input, warn = FALSE)
    data_marker <- grep("^Data:", lines)
    if (length(data_marker) == 0)
      stop(sprintf("Could not find 'Data:' marker in:\n  %s", sed_input))
    data_marker <- data_marker[1]

    df <- utils::read.table(sed_input, skip = data_marker, header = TRUE,
                             sep = "\t", fill = TRUE, check.names = TRUE)
    file_label <- sed_input
  } else if (is.data.frame(sed_input)) {
    df <- sed_input
    file_label <- NA_character_
  } else {
    stop("'sed_input' must be either a character file path to a .sed file or a data.frame")
  }

  if (!"Wvl" %in% names(df))
    stop("'sed_input' must contain a 'Wvl' column")

  if (nrow(df) < 2)
    stop("'sed_input' must contain at least 2 spectral rows")

  reflect_col <- grep("Reflect", names(df), value = TRUE)
  if (length(reflect_col) == 0)
    stop("No column matching 'Reflect' found in 'sed_input'")

  refl_prop <- df[[reflect_col[1]]] / 100
  wvl <- df$Wvl

  integrated <- .trapz_proportion(wvl, refl_prop)
  wvl_min <- min(wvl)
  wvl_max <- max(wvl)

  list(
    mean_reflectance = integrated / (wvl_max - wvl_min),
    wvl_min          = wvl_min,
    wvl_max          = wvl_max,
    n_points         = length(wvl),
    file             = file_label
  )
}
