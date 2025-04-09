# fake mak db

# JOINS TO DO
## Global
### sound_sources - species=code, name (current_sound_source_code also exists)
### call_types - call=code, name (sound_source_code checks against allowed) [optional?]

## Org
### deployments - project=deployment_code, site_code , project_code
### analyses (for effort) - project=deployment_code, analysis_sound_source_codes (species), analysis_start_datetime, analysis_end_datetime

doMakJoins <- function(x, org, global) {
    glob_source <- readxl::read_excel(global, sheet='sound_sources')[
        c('code', 'name', 'current_sound_source_code')
    ]
    names(glob_source) <- c('species_code', 'species', 'current_species_code')
    if('species' %in% names(x)) {
        x <- doMakSpeciesJoin(x, glob_source)
    }
    if('call' %in% names(x)) {
        glob_call <- readxl::read_excel(global, sheet='call_types')[
            c('code', 'name')
        ]
    }

    org_deploy <- readxl::read_excel(org, sheet='deployments')[
        c('deployment_code', 'site_code', 'project_code', 'deployment_latitude', 'deployment_longitude',
          'organization_code')
    ]
    names(org_deploy) <- c('deployment', 'site', 'project', 'depLatitude', 'depLongitude', 'organization')
    if('deployment' %in% names(x)) {
        x <- left_join(x, org_deploy, by='deployment')
        if('Latitude' %in% names(x)) {
            x$Latitude[is.na(x$Latitude)] <- x$depLatitude[is.na(x$Latitude)]
        } else {
            x$Latitude <- x$depLatitude
        }
        if('Longitude' %in% names(x)) {
            x$Longitude[is.na(x$Longitude)] <- x$depLongitude[is.na(x$Longitude)]
        } else {
            x$Longitude <- x$depLongitude
        }
        x$depLatitude <- NULL
        x$depLongitude <- NULL
    }

    org_ana <- readxl::read_excel(org, sheet='analyses', readxl::cell_cols('B:O'))[
        c('deployment_code', 'analysis_sound_source_codes', 'analysis_start_datetime', 'analysis_end_datetime')
    ]
    names(org_ana) <- c('deployment', 'species_code', 'effortStart', 'effortEnd')


    org_ana <- PAMscapes:::spreadEffort(org_ana, commas='species_code')
    org_ana <- doMakSpeciesJoin(org_ana, glob_source)
    if('deployment' %in% names(x)) {
        x <- left_join(x, org_ana, by=c('deployment', 'species'))
    }
    x
}

doMakSpeciesJoin <- function(x, glob_source) {
    if('species' %in% names(x)) {
        x$species_code <- x$species
        x$species <- NULL
    }
    x <- left_join(x, glob_source, by='species_code')
    hasUpdate <- !is.na(x$current_species_code)
    if(any(hasUpdate)) {
        x$species_code[hasUpdate] <- x$current_species_code[hasUpdate]
        x$species <- NULL
        x$current_species_code <- NULL
        x <- left_join(x, glob_source, by='species_code')
    }
    x$current_species_code <- NULL
    noMatch <- is.na(x$species)
    if(any(noMatch)) {
        x$species[noMatch] <- x$species_code[noMatch]
    }
    x$species_code <- NULL
    x
}

org <- '../Data/Acousdet/makDB/Phase 2 Review - Organization-Specific Data - 20241121.xlsx'
glob <- '../Data/Acousdet/makDB/Makara - Phase 2 Review - Global References - 20241121.xlsx'
ana <- readxl::read_excel(org, sheet='analyses', range=readxl::cell_cols('B:O'))
detFiles <- list.files('../Data/Acousdet/SEFSC/', full.names=T)

hm <- loadDetectionData(detFiles, source='makara')
str(hm)
library(dplyr)
joind <- doMakJoins(hm, org=org, global=glob)
joind$site <- gsub('Y[0-9]{1}', '', joind$site)

effs <- distinct(joind[c('effortStart', 'effortEnd', 'species', 'site')])
effs$site <- gsub('Y[0-9]{1}', '', effs$site)
names(effs) <- c('start', 'end', 'species', 'site')

library(patchwork)
scene <- joind %>%
    filter(project =='LISTEN_GOMEX') %>%
    plotAcousticScene(by='site', effort=effs, title='SEFSC LISTEN_GOMEX Example')
scene
boxer <- joind %>%
    filter(project =='LISTEN_GOMEX') %>%
    plotDetectionBoxplot(group='site', facet='species',effort = effs, combineYears = T)
boxer
