import_data_presidente <- function(data_path) {
    library(tidyverse)
    library(readr)
    
    votos <- read_csv(data_path)
    
    votos_presidente <- votos %>%
        mutate(coligacao = sapply(str_extract_all(candidato, "\\b[A-Z]+\\b|PCdoB|PDCdoB|PTdoB"), paste, collapse= ' ')) %>% 
        mutate(partido = gsub("([A-Za-z]+).*", "\\1", coligacao)) %>%
        rowwise() %>% 
        mutate(nome = gsub(paste0("\\", partido, ".*", "|\\("), "", candidato)) %>% 
        na.omit() %>% 
        filter(estado != "TOTAL")
    
    return(votos_presidente)
}

dados_estados <- function() {
    estados_nome <-  c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", "Espirito Santo",
                       "Goiás", "Maranhão", "Mato Grosso", "M. G. do Sul", "Minas Gerais", "Pará", "Paraíba", 
                       "Paraná", "Pernambuco", "Piauí", "R. G. do Norte", "R. G. do Sul", "Rio de Janeiro",
                       "Rondônia", "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins")
    
    estados_sigla <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RN", "RS",
                       "RJ", "RO", "RR", "SC", "SP", "SE", "TO")
    
    estados <- data.frame(estados_nome, estados_sigla, stringsAsFactors = FALSE)
    
    return(estados)
}

dados_presidente_tidy <- function(data_path){
    # data_path - caminho para os dados votos_tidy_long
    votos <- import_data_presidente(data_path)
    
    votos_presidente <- votos %>%
        
        group_by(ano, turno) %>% 
        mutate(total_votos = sum(votos)) %>% 
        mutate(porcentagem = (votos/total_votos) * 100) %>% 
        ungroup() %>% 
        
        select(estado, nome, partido, coligacao, votos, porcentagem, cargo, ano, turno)
    
    return(votos_presidente)
}

dados_presidente_partido <- function(data_path){
    # data_path - caminho para os dados votos_tidy_long
    library(tidyverse)
    library(readr)
    
    votos_presidente <- dados_presidente_tidy(data_path) %>% 
        mutate(porcentagem = round(porcentagem, digits = 2))
    
    return(votos_presidente)
}

dados_presidente_agrupado <- function(data_path) {
    votos <- dados_presidente_tidy(data_path)
    
    votos_agrupado <- votos %>% 
        group_by(partido, ano, turno) %>% 
        summarise(nome = first(nome),
                  votos = sum(votos),
                  porcentagem = sum(porcentagem)) %>% 
        mutate(porcentagem = round(porcentagem, digits = 2))
    
    return(votos_agrupado)
}

dados_presidente_disputa_turno <- function(data_path) {
    
    votos <- dados_presidente_tidy(data_path) %>% 
        mutate(nome = ifelse(grepl("Collor", nome), "Fernando Collor", nome)) %>% 
        mutate(nome = ifelse(grepl("Lula", nome), "Luiz Inácio Lula da Silva", nome)) %>% 
        mutate(nome = ifelse(grepl("Geraldo", nome), "Geraldo Alckmin", nome)) %>% 
        mutate(nome = ifelse(grepl("Serra", nome), "José Serra", nome))

    votos_turno <- votos %>% 
        group_by(nome, ano, turno) %>% 
        summarise(votos = sum(votos),
                  porcentagem = round(sum(porcentagem), digits = 2)) %>% 
        ungroup() %>% 
        group_by(ano, nome) %>% 
        mutate(n = n()) %>% 
        filter(n > 1) %>% 
        select(-n) %>% 
        ungroup() %>% 
        mutate(candidato = nome) %>% 
        mutate(nome = paste0(ano, " - ", nome)) %>% 
        arrange(nome)
    
    return(votos_turno)
}

dados_presidente_turno_estados <- function(data_path) {
    
    votos <- import_data_presidente(data_path) %>% 
        mutate(nome = ifelse(grepl("Collor", nome), "Fernando Collor", nome)) %>% 
        mutate(nome = ifelse(grepl("Lula", nome), "Luiz Inácio Lula da Silva", nome)) %>% 
        mutate(nome = ifelse(grepl("Geraldo", nome), "Geraldo Alckmin", nome)) %>% 
        mutate(nome = ifelse(grepl("Serra", nome), "José Serra", nome)) %>% 
        mutate(estado = ifelse(grepl("Federal", estado), "Distrito Federal", estado)) %>% 
        mutate(estado = ifelse(grepl("Janeiro", estado), "Rio de Janeiro", estado)) %>% 
        mutate(estado = ifelse(grepl("Santo", estado), "Espírito Santo", estado))
    
    votos_turno <- votos %>% 
        group_by(estado, ano, turno) %>% 
        mutate(total_votos_estado = sum(votos)) %>% 
        ungroup() %>% 
        
        group_by(estado, nome, ano, turno) %>% 
        mutate(porcentagem = round((votos/total_votos_estado) * 100, digits = 2)) %>%
        ungroup() %>% 
        
        group_by(estado, nome, ano) %>% 
        mutate(n = n()) %>% 
        filter(n > 1) %>% 
        select(-n) %>% 
        ungroup() %>% 
        
        mutate(candidato = nome) %>% 
        mutate(nome = paste0(ano, " - ", nome)) %>% 
        mutate(candidato_estado = paste0(estado, " - ", nome)) %>% 
        arrange(nome) %>% 
        select(estado, candidato, candidato_estado, votos, ano, turno, nome, porcentagem)
    
    return(votos_turno)
}

dados_presidente_estado_2014 <- function(data_path) {
    
    votos <- import_data_presidente(data_path)
    
    votos_presidente <- votos %>%
        mutate(estado = ifelse(grepl("Federal", estado), "Distrito Federal", estado)) %>% 
        mutate(estado = ifelse(grepl("Janeiro", estado), "Rio de Janeiro", estado)) %>% 
        select(estado, ano, turno, partido, votos)
        
    votos_presidente_estado <- votos_presidente %>% 
        filter(partido %in% c("PT", "PSDB")) %>% 
        filter(ano == 2014, turno == "Turno 2") %>% 
        
        group_by(estado) %>% 
        mutate(total = sum(votos)) %>% 
        mutate(porcentagem = round((votos / total) * 100, digits = 2)) %>% 
        ungroup()

    estados <- dados_estados()
    
    votos_estado_sigla <- votos_presidente_estado %>% 
        left_join(estados, by = c("estado" = "estados_nome")) %>% 
        mutate(UF = estados_sigla)
    
    votos_spread <- votos_estado_sigla %>% 
        spread(partido, porcentagem) %>% 
        mutate(PSDB = ifelse(is.na(PSDB), 0, PSDB)) %>% 
        mutate(PT = ifelse(is.na(PT), 0, PT)) %>% 
        
        group_by(estado) %>% 
        summarise(PSDB = sum(PSDB),
                  PT = sum(PT)) %>% 
        mutate(diferenca = PT - PSDB)
    
    votos_estado_ano <- votos_estado_sigla %>% 
        left_join(votos_spread %>% select(estado, diferenca), by = ("estado"))
    
    return(votos_estado_ano)
}

export_data <- function() {
    library(here)
    data_path <- here("data/votos_tidy_long.csv")
    
    votos <- dados_presidente_partido(data_path)
    write.csv(votos, here("data/votos_presidente_partido.csv"), row.names = FALSE)
    
    votos_agrupado <- dados_presidente_agrupado(data_path)
    write.csv(votos_agrupado, here("data/votos_presidente_agrupado.csv"), row.names = FALSE)
    
    votos_turno <- dados_presidente_disputa_turno(data_path)
    write.csv(votos_turno, here("data/votos_presidente_segundo_turno.csv"), row.names = FALSE)
    
    votos_turno_estado <- dados_presidente_turno_estados(data_path)
    write.csv(votos_turno_estado, here("data/votos_presidente_turno_estado.csv"), row.names = FALSE)
    
    votos_estado_ano <- dados_presidente_estado_2014(data_path)
    write.csv(votos_estado_ano, here("data/votos_presidente_estado_ano.csv"), row.names = FALSE)
    
}

