# APP_Dashboard_em_Shiny_R

O Aplicativo foi desenvolvido em Shiny (R) durante o período de estágio como Estatísitco/Ciêntista de Dados na Empresa COMVEST - UNICAMP.

Como o banco de dados utilizados no APP/Dashboard não é aberto ao público, este repositório conta apenas com o script do aplicativo.

pacman::p_load(shinyWidgets,fresh,shinyBS,tidyverse,RMariaDB,janitor,shiny,DT,shinythemes,bslib,stringr,fst,paletteer,rsconnect,data.table,dtplyr,bs4Dash,shinydashboard,shinydashboardPlus,waiter,lubridate,highcharter,geojsonio,leaflegend,leaflet,readxl,readr)



## Carregando os dados:----
dados = setDT(fst::read.fst("dados_app.fst"))

ano_atual = as.numeric(as.character(dados[order(-Ano),unique(Ano)][1]))

# cria a negação da função %in%
`%!in%` <- Negate(`%in%`)

# LISTA PARA OS INPUTS DA ABA DICIONARIO 
{testando = list(
  "Cor" = paste("<p> <b>
                    Variável:</b> Cor <p>  <p><b>
                    Observação: </b> Informação coletada a partir do vestibular 2003. <br> <b>
                    Valores: </b> Branca, Preta, Parda, Indígena, Amarela, Não declarada e Em branco.<p>"),
  "Processo" = paste("<p> <b>
                          Variável:</b> Tipo Ingresso <p> <p> <b>
                          Observação: :</b> Os dados socioeconômicos dos egresso do ProFIS se referem aos dados de inscrição no momento de ingresso no ProFIS.<b>  <br>
                          Valores: </b> Vestibular Unicamp, ENEM-Unicamp, Vestibular Indígena, Vagas Olímpicas, ProFIS, Egresso ProFIS.
                         <p>"),
  "Forma_Ingresso" = paste("<b>
                               Variável: </b>Grupos de Ingresso<b><br>
                               Valores: </b><br>
                                  <b>VU AC: </b> Ingresso pelo Vestibular Unicamp (VU) sem utilização de alguma das políticas de inclusão, PAAIS ou cotas étnico-raciais.
                                  <b>VU AC+PAAIS: </b> Ingresso pelo VU utilizando o PAAIS, porém sem a bonificação para autodeclarados pretos, pardos ou indígenas (de 2005 a 2018) ou a utilização das cotas étnico-raciais (a partir de 2019).
                                  <b>VU AC+PAAIS+PPI: </b> Ingresso pelo VU utilizando o PAAIS com a bonificação para escola pública e para autodeclarados pretos, pardos ou indígenas (de 2005 a 2018).
                                  <b>VU Cotas: </b>Ingresso pelo VU via as cotas étnico raciais, porém sem a utilização do PAAIS.
                                  <b>VU Cotas+PAAIS: </b> Ingresso pelo VU via as cotas étnico raciais com a bonificação PAAIS (escola pública).<br>
                                  <b>ENEM EP: </b> Ingresso pelo Edital ENEM-Unicamp, candidatos escola pública.
                                  <b>ENEM EP+PPI: </b>Ingresso pelo Edital ENEM-Unicamp, candidatos de escola pública e autodeclarados pretos, pardos ou indígenas.
                                  <b>ENEM PP:  </b>Ingresso pelo Edital ENEM-Unicamp, candidatos autodeclarados pretos ou pardos sem a obrigatoriedade de ter estudado em escola pública (edições de 2019, 2020 e 2022).<br>
                                  <b>VO:  </b> Ingresso pelo Edital Vagas Olímpicas.<br>
                                  <b>VI:  </b> Ingresso pelo Vestibular Indígena.<br>
                                  <b>ProFIS: </b>  Ingresso no ProFIS.
                                  <b>Egresso ProFIS: </b> Alunos concluintes/egressos do ProFIS ingressos em um curso de graduação."),
  "Sexo" = paste("<p> <b>
                     Variável:</b> Sexo <p> <p><b> 
                     Valores: </b>Masculino ou Feminino.<p>"),
  "Idade" = paste("<p> <b>
                      Variável:</b> Idade <p> <p><b> 
                      Valores: </b> Até 17 anos, 18 a 20 anos, 21 a 23 anos, 24 a 29 anos e Mais de 29 anos.<p>"),
  "Ens_fund" = paste("<p> <b>
                         Variável:</b> Ensino Fundamental <p> <p> <b>
                         Descrição: </b> Tipo de escola onde o candidato cursou o ensino fundamental  <br><b> 
                         Valores:</b>  Público, Público/Privado, Privado, Outro, Em branco e Sem Informação.<p>"),
  "Ens_fund_1" = paste("<p> <b>
                           Variável:</b> Ensino Fundamental I <p>  <p> <b>
                           Descrição: </b> Tipo de escola onde o candidato cursou os anos iniciais do ensino fundamental (ensino fundamental I) <br> <b> 
                           Observação:</b> Informação coletada a partir de 2017. <b> <br>
                           Valores:</b>  Público, Público/Privado, Privado, Outro, Em branco e Sem Informação.<p>"),
  "Ens_fund_2" = paste("<p> <b>
                           Variável:</b> Ensino Fundamental II <p>  <p> <b>
                           Descrição: </b> Tipo de escola onde o candidato cursou os anos iniciais do ensino fundamental (ensino fundamental II)<br><b> 
                           Observação:</b> <br> Informação coletada a partir de 2017.<b> <br>
                           Valores:</b> Público, Público/Privado, Privado, Outro, Em branco e Sem Informação. <p>"),
  "Ens_med" = paste("<p> <b>
                        Variável: </b>Ensino Médio <p>  <p> <b>
                        Descrição:</b> Tipo de escola onde o candidato cursou o ensino médio  <br> <b> 
                        Valores: </b> Público, Público/Privado, Privado, Outro, e Em branco.<p>"),
  "Tecnico" = paste("<p> <b>
                        Variável:</b> Colégio Técnico <p>  <p> <b>
                        Descrição:</b> Se o ensino médio foi em Colégio Técnico ou não. <br> <b>
                        Valores: </b> Técnico, Outro, Em branco.<p>"),
  "Residencia" = paste("<p> <b>
                            Variável:</b> Local de Residência  <p>  <p> <b>
                            Descrição:</b> Local de residência. <br> <b>
                            Valores: </b> <br>
                              <b>RMC: </b> Região metropolitana de Campinas. <br> 
                              <b>RMSP: </b> Região metropolitana de São Paulo. <br>
                              <b>Outra região de SP</b> <br>
                              <b>Outros Estados</b> <br>
                              <b>Em branco</b>
                              <p>"),
  "Isento" = paste("<p> <b>
                       Variável:</b> Isento <p>  <p> <b>
                       Descrição:</b> Se o candidato participou ou não do programa de isenção da taxa de inscrição para o Vestibular Unicamp e Edital ENEM-Unicamp. (não se aplica as demais formas de ingresso) <br><b>
                       Valores: </b>Isento, Não.<p>"),
  "Inst_mae" = paste("<p> <b>
                         Variável:</b> Instrução Mãe<p>  <p> <b>
                         Descrição:</b> Nível de Instrução da mãe. <br> <b>
                         Valores: </b> <br>
                            Não Estudou <br>
                            Fund Inc: Ensino fundamental incompleto. <br> 
                            Fund C: Ensino fundamental completo. <br>
                            Médio Inc: Ensino médio incompleto. <br>
                            Médio C: Ensino médio completo <br>
                            Sup Inc: Ensino superior incompleto<br>
                            Sup C: Ensino superior completo <br>
                            Pós Inc: Pós graduação incompleta<br>
                            Pós C: Pós graduação completa<br>
                            Em branco<p>"),
  "Inst_pai" = paste("<p> <b>
                         Variável: </b>Instrução Pai<p>  <p> <b>
                         Descrição:</b> Nível de Instrução do Pai. <br> <b>
                         Valores: </b>
                            Não Estudou
                            Fund Inc: Ensino fundamental incompleto. <br> 
                            Fund C: Ensino fundamental completo. <br>
                            Médio Inc: Ensino médio incompleto. <br>
                            Médio C: Ensino médio completo <br>
                            Sup Inc: Ensino superior incompleto<br>
                            Sup C: Ensino superior completo <br>
                            Pós Inc: Pós graduação incompleta<br>
                            Pós C: Pós graduação completa<br>
                            Em branco<p>"),
  "Ocup_mae" = paste("<p> <b>
                         Variável: </b>Ocupação Mãe<p>  <p> <b>
                         Descrição:</b> Ocupação da Mãe <br> <b>
                         Valores: </b> <br>
                            I: Ocupações do lar <br>
                            II: Ocupações manuais não especializadas <br>
                            III: Ocupações manuais especializadas e assemelhadas <br>
                            IV: Supervisão de trabalho manual e ocupações assemelhadas <br>
                            V: Ocupações não manuais de rotina <br>
                            VI: Supervisão de ocupações técnicas ou assemelhadas <br>
                            VII: Profissionais liberais, cargos médios de gerência e direção <br>
                            VIII: Proprietárias e altos cargos políticos e/ou administrativos <br>
                            Desempregada <br>
                            Outra <br>
                            Em branco <br>
                            <p>"),
  "Ocup_pai" = paste("<p> <b>
                         Variável: </b>Ocupação Pai<p>  <p> <b>
                         Descrição:</b> Ocupação do Pai <br> <b>
                         Valores: </b> <br>
                            I: Ocupações do lar <br>
                            II: Ocupações manuais não especializadas <br>
                            III: Ocupações manuais especializadas e assemelhadas <br>
                            IV: Supervisão de trabalho manual e ocupações assemelhadas <br>
                            V: Ocupações não manuais de rotina <br>
                            VI: Supervisão de ocupações técnicas ou assemelhadas <br>
                            VII: Profissionais liberais, cargos médios de gerência e direção <br>
                            VIII: Proprietárias e altos cargos políticos e/ou administrativos <br>
                            Desempregado <br>
                            Outra <br>
                            Em branco <br>
                            <p>"),
  "Renda_total" = paste("<p> <b>
                            Variável:</b> Renda <p>  <p> <b>
                            Descrição:</b> Renda familiar mensal total em salários mínimos.  <br> <b>
                            Observação:</b> Não há essa informação para os inscritos/ingressantes de 2011. <br><br> <b>
                            Valores: </b> <br>
                              Até 1 Salário mínimo.<br>
                              1 a 3 Salários mínimos. <br>
                              3 a 5  Salários mínimos. <br>
                              5 a 10 Salários mínimos.<br>
                              10 a 15 Salários mínimos. <br>
                              15 a 20 Salários mínimos. <br>
                              Mais de 20 Salários mínimos.<br>
                              Em branco.<p>"),
  "Cursinho" = paste("<p> <b>
                         Variável:</b> Cursinho <p>  <p> <b>
                         Descrição:</b> Fez cursinho?  <br> <b>
                         Valores: </b> Sim, Não, Em branco. <p>"),
  "Est_civil" = paste("<p> <b>
                          Variável: Estado Civil </b><p>  <p> <b>
                          Valores: </b> Solteiro, Casado, Separado, Viúvo, Outro e Em branco.<p>"),
  "Part_econ" = paste("<p> <b>
                          Variável:</b> Participação Econômica<p>  <p> <b>
                          Descrição:</b> Participação econômica na família  <b>  <br>
                          Valores: </b> Não Trabalha, Trabalha e recebe ajuda, Trabalha e ajuda a família, Trabalha e sustenta a família, Em branco.<p>"),
  "Atv_remunerada" = paste("<p> <b>
                          Variável: Atividade Remunerada </b><p>  <p> <b>
                          Descrição:</b> Se Exerce Atividade Remunerada  <b><br>
                          Valores: </b> <br>
                          Sim, Tempo Integral. <br>
                          Sim, Semi-Integral.<br>
                          Sim, Trabalho Eventual.<br>
                          Não Exerce.<br>
                          Em branco.<p>"),
  "Contribuem_renda" = paste("<p> <b>
                                 Variável:</b> Contribuem com a Renda<p>  <b> <p>
                                 Descrição:</b> Quanta pessoas contribuem para a renda familiar? <b> <br>
                                 Valores: </b> 1, 2, 3, 4 ou mais, Em branco.<p>"),
  "Dependentes_renda" = paste("<p> <b>
                                  Variável: </b>Dependentes Renda <p>  <b> <p>
                                  Descrição:</b> Quantas pessoas dependem da renda familiar?  <b> <br>
                                  Valores: </b> 1, 2, 3, 4 5, 6 ou mais, Em branco<p>"),
  "Periodo_ens_med" = paste("<p> <b>
                                Variável:</b> Período Ensino Médio <p>  <p> <b> 
                                Valores: </b> Diurno, Integral, Diurno/Norturno, Noturno, Outro e Em branco.<p>"),
  "Sit_moradia" = paste("<p> <b>
                            Variável:</b> Situação Moradia <p>  <p> <b>
                            Valores: </b> Imóvel próprio quitado, Imóvel próprio não quitado, Imóvel alugado, Imóvel cedido, Outra situação e Em branco.<p>"),
  "Mot_unicamp" = paste("<p> <b>
                            Variável:</b> Motivação Unicamp<p>  <p> <b> 
                            Valores: </b> </b> <br>
                              A riqueza cultural de sua vida universitária <br>
                              Acesso a carreira científica <br> 
                              É a escolhida pela maioria dos meus amigos <br> 
                              É a instituição que oferece o melhor curso de minha escolha <br>
                              É a mais próxima de minha residência <br>
                              O conceito de que desfruta como universidade <br> 
                              Oferece ensino gratuito <br>
                              Outro <br> 
                              Em branco <br>
                            <p>")
  
)
}

### Tema para os gráfico:
tema = hc_theme_flat(
  title = list(
    style = list(
      fontFamily = "Franklin Gothic Medium",
      fontSize = "25px")),
  subtitle = list(
    style = list(
      fontFamily = "Georgia",
      fontSize = "15px")))
###

# MAPA
lads <- geojson_read(
  x ='brasil.geojson',what = "sp")

uf <- geojson_read(
  x = 'uf.geojson', what = "sp")

# Contornos:
cps = subset(lads, name == 'Campinas')
sp = subset(uf, UF_05 == 'SP')
br = subset(uf, MICRO == 'BRASILEIA')

#Carregando dados de Latitude e longitude das cidades de inscrição.
lat_lon = read_xlsx("latlon.xlsx")

#Ícones para o mapa:
IconSet <- awesomeIconList(
  `Vestibular Unicamp` = makeAwesomeIcon(icon = 'education', library = 'glyphicon', markerColor = 'blue', iconColor = 'black'),
  `Vestibular Indígena` = makeAwesomeIcon(icon = 'education', library = 'glyphicon', markerColor = 'green', iconColor = 'black'))


# VARIAVEIS QUE APARECEM NO INPUT DA VARIAVEL DE REFERENCIA DO GRAFICO - ABA: "PERFIL SOCIECONOMICO" 
opcoes_variavel_ref_perfil = c("Cor","Tipo de Ingresso"="Processo","Grupos de Ingresso" = "Forma_Ingresso","Sexo","Idade","Ensino Fundamental" = "Ens_fund","Ensino Fundamental 1"="Ens_fund_1","Ensino Fundamental 2" = "Ens_fund_2","Ensino Médio" = "Ens_med","Colégio Técnico" ="Tecnico","Local de Residencia" ="Residencia","Isento","Instrução Mãe" = "Inst_mae" ,"Ocupação Mãe" = "Ocup_mae","Instrução Pai" ="Inst_pai","Ocupação Pai" = "Ocup_pai","Renda" ="Renda_total","Cursinho" ,"Estado Civil" = "Est_civil","Participação Econômica" = "Part_econ", "Atividade Remunerada" = "Atv_remunerada", "Contribuem com Renda" = "Contribuem_renda" , "Dependentes Renda" = "Dependentes_renda", "Periodo Ensino Médio" = "Periodo_ens_med","Situação Moradia" = "Sit_moradia","Motivação Unicamp" = "Mot_unicamp")

# VARIAVEIS QUE APARECEM NO INPUT DA VARIAVEL DO GRAFICO DA SERIE HISTORICA - ABA: "SERIE HISTORICAO" 
opcoes_variavel_serie = c("Cor","Tipo de Ingresso"="Processo","Grupos de Ingresso" = "Forma_Ingresso","Sexo","Idade","Ensino Fundamental" = "Ens_fund","Ensino Fundamental 1"="Ens_fund_1","Ensino Fundamental 2" = "Ens_fund_2","Ensino Médio" = "Ens_med","Colégio Técnico" ="Tecnico","Local de Residencia" ="Residencia" ,"Isento","Instrução Mãe" = "Inst_mae" ,"Ocupação Mãe" = "Ocup_mae","Instrução Pai" ="Inst_pai","Ocupação Pai" = "Ocup_pai","Renda" ="Renda_total","Cursinho","Estado Civil" = "Est_civil","Participação Econômica" = "Part_econ", "Atividade Remunerada" = "Atv_remunerada","Contribuem com Renda" = "Contribuem_renda" , "Dependentes Renda" = "Dependentes_renda", "Período Ensino Médio" = "Periodo_ens_med","Situação Moradia" = "Sit_moradia","Motivação Unicamp" = "Mot_unicamp" )

# OPCOES QUE APARECEM NOS INPUTS DOS FILTROS
load("opcoes_input.RDATA")

# INTERFACE DO APP
ui = bs4DashPage( dark = NULL,
                  
                  
                  header = 
                    # ABAS DO APP
                    bs4DashNavbar(
                      title = dashboardBrand(title = "Dashboard Comvest", opacity = 1,color = "danger",href = "https://www.comvest.unicamp.br/estatisticas-comvest/estatisticas-sociais/perfil-socioeconomico/perfil-socioeconomico-geral/",image = "https://media-exp1.licdn.com/dms/image/C560BAQGEfA5ymZLTKA/company-logo_200_200/0/1540677559920?e=2147483647&v=beta&t=QFMa53U9GX_e1JphjMQhYreErKwnqx5R0rYrHONxwTg"),
                      lefttUi = tags$li(class="navbar-collapse collapse dropdown",tags$ul(class="nav navbar-nav sidebar-menu",
                                                                                          # MENUS SUPERIORES -------------------------------------------------------------------------------------------------------- 
                                                                                          bs4SidebarMenuItem("Informações Gerais", tabName="pag1"),
                                                                                          bs4SidebarMenuItem("Dicionário", tabName="pag2"),
                                                                                          bs4SidebarMenuItem("Perfil Socioeconômico",tabName = "perfil_insc"),
                                                                                          bs4SidebarMenuItem("Série Histórica",tabName = "serie_insc"),
                                                                                          bs4SidebarMenuItem("Inscritos e Suas Formas de Ingresso",tabName = "forma_ingr_insc"),
                                                                                          bs4SidebarMenuItem("Mapa Locais de Prova",tabName = "mapa_prova"),
                                                                                          style="font-size: 1em;"
                      )),sidebarIcon = tags$ul(class = "navbar-nav",
                                               style="width: 0px;font-size:0px;" )
                      # MENUS SUPERIORES --------------------------------------------------------------------------------------------------------
                    ),
                  
                  sidebar = bs4DashSidebar(disable = T,minified = F),
                  
                  body = bs4DashBody(  useWaiter(), 
                                       waiterShowOnLoad(html = spin_6(),color = "rgba(13,32,43,0.27)"),
                                       waiterOnBusy(html = spin_6(),color = "rgba(13,32,43,0.27)"),
                                       use_theme(create_theme(bs4dash_color(red = "#8f2121"),bs4dash_vars("card-title-font-size"= "1.5rem","progress-bar-border-radius"="2px","body-bg"="rgba(2,58,79,0.71)"))),
                                       tabItems(
                                         # ABA PERFIL SOCIECONOMICO 
                                         tabItem(tabName = "perfil_insc",
                                                 # FILTROS ----------------------------------------------------------------------------------------------------
                                                 bs4Card(id = "card1_perfil_insc",width = 12,elevation = 4,maximizable = T,status = "danger",label = "(Clique no +/- para abrir/fechar)",title = "Filtros",
                                                         solidHeader = T,collapsed = F,headerBorder = F,
                                                         fluidRow(
                                                           pickerInput(
                                                             inputId = "ano",
                                                             label = "Ano",
                                                             choices = sort(levels(dados[,Ano]),decreasing = T),
                                                             options = pickerOptions(
                                                               `actions-box` = TRUE,
                                                               liveSearch = TRUE,
                                                               title = "Selecione o(s) Ano(s)"
                                                               #selectedTextFormat = "static"
                                                             ),
                                                             multiple = F,
                                                             selected = ano_atual
                                                           ),
                                                           pickerInput(
                                                             inputId = "area",
                                                             label = "Area",
                                                             choices =  levels(dados[,area_insc]),
                                                             options = pickerOptions(
                                                               `actions-box` = TRUE,
                                                               liveSearch = TRUE,
                                                               title = "Selecione a(s) Area(s)"
                                                               #selectedTextFormat = "static"
                                                             ),
                                                             multiple = T
                                                           ),
                                                           pickerInput(
                                                             inputId = "cur_insc",
                                                             label = "Curso",
                                                             choices = levels(dados[,cur_insc]),
                                                             options = pickerOptions(
                                                               `actions-box` = TRUE,
                                                               liveSearch = TRUE,
                                                               title = "Selecione o(s) Curso(s)"
                                                               #selectedTextFormat = "static"
                                                             ),
                                                             multiple = T
                                                           ),
                                                           pickerInput(
                                                             inputId = "processo",
                                                             label = "Tipo de Ingresso",
                                                             choices = levels(dados[,Processo]),
                                                             options = pickerOptions(
                                                               `actions-box` = TRUE,
                                                               liveSearch = TRUE,
                                                               title = "Selecione o(s) Tipo(s) de Ingresso"
                                                               #selectedTextFormat = "static"
                                                             ),
                                                             multiple = T,
                                                             width = '280px'
                                                           ),
                                                           pickerInput(
                                                             inputId = "forma_ingr",
                                                             label = "Grupo de Ingresso",
                                                             choices =  levels(dados[,Forma_Ingresso]),
                                                             options = pickerOptions(
                                                               `actions-box` = TRUE,
                                                               liveSearch = TRUE,
                                                               title = "Selecione o(s) Grupo(s) de Ingresso"
                                                               #selectedTextFormat = "static"
                                                             ),
                                                             multiple = T,
                                                             width = '280px'
                                                           ),
                                                           pickerInput(
                                                             inputId = "isento",
                                                             label = "Isento do Pagamento da Inscricão",
                                                             choices = levels(dados[,Isento]),
                                                             options = pickerOptions(
                                                               `actions-box` = TRUE,
                                                               liveSearch = TRUE,
                                                               title = "Selecione uma Opção"
                                                               #selectedTextFormat = "static"
                                                             ),
                                                             multiple = T,
                                                             width = '280px'
                                                           ),
                                                           pickerInput(
                                                             inputId = "escola",
                                                             label = "Tipo de Ensino Médio",
                                                             choices =  levels(dados[,Ens_med]),
                                                             options = pickerOptions(
                                                               `actions-box` = TRUE,
                                                               liveSearch = TRUE,
                                                               title = "Selecione os(s) Tipo(s) de Ensino"
                                                               #selectedTextFormat = "static"
                                                             ),
                                                             multiple = T,
                                                             width = '280px'
                                                           ),
                                                           pickerInput(
                                                             inputId = "sexo",
                                                             label = "Sexo",
                                                             choices =  levels(dados[,Sexo]),
                                                             options = pickerOptions(
                                                               `actions-box` = TRUE,
                                                               liveSearch = TRUE,
                                                               title = "Selecione o(s) Sexo(s)"
                                                               #selectedTextFormat = "static"
                                                             ),
                                                             multiple = T
                                                           ),
                                                           pickerInput(
                                                             inputId = "cor",
                                                             label = "Autodeclaração de Cor",
                                                             choices =  levels(dados[,Cor]),
                                                             options = pickerOptions(
                                                               `actions-box` = TRUE,
                                                               liveSearch = TRUE,
                                                               title = "Selecione a(s) Cor(s)"
                                                               #selectedTextFormat = "static"
                                                             ),
                                                             multiple = T
                                                           ),
                                                           actionBttn(
                                                             inputId = "reset_input",
                                                             label = HTML("<h4>Limpar Filtros</h4> <span style='font-size:14px'> (Limpe os filtros sempre que realizar nova consulta) </span>"),
                                                             style = "fill", 
                                                             color = "primary",
                                                             size = "xs"
                                                           )
                                                           #actionButton("reset_input", "Limpar Filtros",style="position:relative;height:40px;bottom:-1.95rem;",width = "150px")
                                                         )
                                                         # FILTROS ----------------------------------------------------------------------------------------------------
                                                 ),
                                                 # CAIXAS COM OS INCRITOS E MATRICULADOS
                                                 fluidRow(
                                                   bs4ValueBoxOutput(outputId = "p_pppi",width = 2),
                                                   bs4ValueBoxOutput(outputId = "p_indigena",width = 2),
                                                   bs4ValueBoxOutput(outputId = "p_feminino",width = 2),
                                                   bs4ValueBoxOutput(outputId = "p_escpub",width = 2),
                                                   bs4ValueBoxOutput(outputId = "p_tecnico",width = 2),
                                                   bs4ValueBoxOutput(outputId = "p_cursinho",width = 2)
                                                 ),
                                                 # CAIXA "Seleção e Cruzamento de Variáveis"
                                                 fluidRow(
                                                   
                                                   bs4Card(width = 6,title = div(HTML('Seleção e Cruzamento de Variáveis')),elevation = 4,maximizable = T,status = "danger", solidHeader = T,collapsed = F,headerBorder = F,
                                                           column(width = 12,offset = 1,
                                                                  fluidRow(
                                                                    selectInput("variavel_perfil",label = "Selecionar uma variável de referência",multiple = F,selected = "Cor",
                                                                                choices = opcoes_variavel_ref_perfil,width = '350px'),
                                                                    selectInput("cruzamento_perfil",label = "Selecionar uma variável para comparação",multiple = F,selected = "Nenhuma",
                                                                                choices = c("Nenhuma",opcoes_variavel_ref_perfil,width = '350px'))
                                                                  ))),
                                                   bs4ValueBoxOutput(outputId = "p_inscritos",width = 3),
                                                   bs4ValueBoxOutput(outputId = "p_isento",width = 3)
                                                 ),
                                                 # MUDA ALGUNS ELEMENTOS EM HTML CSS DO APP
                                                 fluidRow(tags$style(HTML(
                                                   ".highcharts-figure,

.highcharts-data-table table{
  min-width: 320px;
  max-width: 800px;
  margin: 1em auto;
}

.card-body{
  height: auto;
}


.nav{
  #text-decoration-line: underline;
}

.navbar-light .navbar-nav .nav-link.active {
    color: #000000;
    border-style: double;
    border-radius: 5px;
    background-color: #383c561c;
    border-width: thin;
    padding: 7.1px;
}



.layout-top-nav .wrapper .main-header .brand-image {
    height: 43px;
    max-height: 43px;
}

.font-weight-light {
  margin-left: 0.5em;
  margin-right: 0.5em;
  font-weight: 400 !important;
}

.highcharts-background{
   fill: white;
}

.highcharts-caption p{
 font-size: .9rem;
}

.shiny-html-output shiny-bound-output h6,p{
  font-size: 1.1em;
}

.highcharts-data-table table {
  font-family: Verdana, sans-serif;
  border-collapse: collapse;
  border: 1px solid #ebebeb;
  margin: 10px auto;
  text-align: center;
  width: 100%;
  max-width: 500px;
}

.highcharts-data-table caption {
  padding: 1em 0;
  font-size: .2em;
  color: #555;
}

.highcharts-data-table th {
  font-weight: 600;
  padding: 0.5em;
}

.highcharts-data-table td,
.highcharts-data-table th,
.highcharts-data-table caption {
  padding: 0.5em;
}

.highcharts-data-table thead tr,
.highcharts-data-table tr:nth-child(even) {
  background: #f8f8f8;
}

.highcharts-data-table tr:hover {
  background: #f1f7ff;
}

")),# GRÁFICOS ABA DE PERFIL
bs4Card(width = 6,title = div(HTML('Inscritos')),elevation = 4,maximizable = T,status = "danger", solidHeader = T,collapsed = F,headerBorder = F,
        uiOutput("perfil_inscritos")
),
bs4Card(width = 6,title = div(HTML("Matriculados")),elevation = 4,maximizable = T,status = "danger", solidHeader = T,collapsed = F,headerBorder = F,
        uiOutput("perfil_matriculados")
)
                                                 )
                                         ),
# ABA DE SERIE HISTORICA
tabItem(tabName = "serie_insc",
        # INPUTS ABA DE SERIE HISTORICA
        bs4Card(width = 12,title = div(HTML('Seleção de Variável e Filtros')),elevation = 4,maximizable = T,status = "danger", solidHeader = T,collapsed = F,headerBorder = F,
                fluidRow(
                  selectInput("variavel_serie", width = "15%",label = "Selecionar uma variável",multiple = F,selected = "Cor",
                              choices = opcoes_variavel_serie
                  ),
                  #selectInput("tipo_grafico_serie",width = "10%",label = "Tipo de gráfico",multiple = F,selected = "Gráfico de Linhas", choices = c("Gráfico de Linhas","Gráfico de Barras")),
                  radioGroupButtons(
                    inputId = "tipo_graf_serie",
                    label = "Tipo de Gráfico", 
                    choices = c( `<i class='fa fa-line-chart'></i>` = "line",`<i class='fa fa-bar-chart'></i>` = "bar"),
                    justified = TRUE,selected = "line",status = "primary",width = "10%",checkIcon = list(
                      yes = icon("ok", 
                                 lib = "glyphicon"))
                  ),
                  # FILTROS ----------------------------------------------------------------------------------------------------
                  pickerInput(
                    inputId = "area_s",
                    label = "Area",
                    choices =  levels(dados[,area_insc]),
                    options = pickerOptions(
                      `actions-box` = TRUE,
                      liveSearch = TRUE,
                      title = "Selecione a(s) Area(s)"
                      #selectedTextFormat = "static"
                    ),
                    multiple = T,
                    width = '190px'
                  ),
                  pickerInput(
                    inputId = "cur_insc_s",
                    label = "Curso",
                    choices = list(
                      "Artes" = levels(dados$cur_insc[which(dados$area_insc == "Artes")])[unique(dados$cur_insc[which(dados$area_insc == "Artes")])] %>% sort(),
                      "Ciências Biológicas" = levels(dados$cur_insc[which(dados$area_insc %in% c("Ciências Biológicas","Medicina"))])[unique(dados$cur_insc[which(dados$area_insc %in% c("Ciências Biológicas","Medicina"))])] %>% sort(),
                      "Ciências Exatas" = levels(dados$cur_insc[which(dados$area_insc == "Ciências Exatas")])[unique(dados$cur_insc[which(dados$area_insc == "Ciências Exatas")])] %>% sort(),
                      "Ciências Humanas" = levels(dados$cur_insc[which(dados$area_insc == "Ciências Humanas")])[unique(dados$cur_insc[which(dados$area_insc == "Ciências Humanas")])] %>% sort(),
                      "Engenharias" = levels(dados$cur_insc[which(dados$area_insc == "Engenharias")])[unique(dados$cur_insc[which(dados$area_insc == "Engenharias")])] %>% sort(),
                      "Tecnológias" = levels(dados$cur_insc[which(dados$area_insc == "Tecnológias")])[unique(dados$cur_insc[which(dados$area_insc == "Tecnológias")])] %>% sort(),
                      "Profis" = levels(dados$cur_insc[which(dados$area_insc == "Profis")])[unique(dados$cur_insc[which(dados$area_insc == "Profis")])] %>% sort()
                    ),
                    options = pickerOptions(
                      `actions-box` = TRUE,
                      liveSearch = TRUE,
                      title = "Selecione o(s) Curso(s)"
                      #selectedTextFormat = "static"
                    ),
                    multiple = T,
                    width = '190px'
                  ),
                  pickerInput(
                    inputId = "processo_s",
                    label = "Tipo de Ingresso",
                    choices = levels(dados[,Processo]),
                    options = pickerOptions(
                      `actions-box` = TRUE,
                      liveSearch = TRUE,
                      title = "Selecione o(s) Tipo(s) de Ingresso"
                      #selectedTextFormat = "static"
                    ),
                    multiple = T,
                    width = '280px'
                  ),
                  pickerInput(
                    inputId = "isento_s",
                    label = "Isento do Pagamento da Inscricão",
                    choices = levels(dados[,Isento]),
                    options = pickerOptions(
                      `actions-box` = TRUE,
                      liveSearch = TRUE,
                      title = "Selecione uma Opção"
                      #selectedTextFormat = "static"
                    ),
                    multiple = T,
                    width = '280px'
                  ),
                  pickerInput(
                    inputId = "sexo_s",
                    label = "Sexo",
                    choices =  levels(dados[,Sexo]),
                    options = pickerOptions(
                      `actions-box` = TRUE,
                      liveSearch = TRUE,
                      title = "Selecione o(s) Sexo(s)"
                      #selectedTextFormat = "static"
                    ),
                    multiple = T,
                    width = '190px'
                  ),
                  pickerInput(
                    inputId = "cor_s",
                    label = "Autodeclaração de Cor",
                    choices =  levels(dados[,Cor]),
                    options = pickerOptions(
                      `actions-box` = TRUE,
                      liveSearch = TRUE,
                      title = "Selecione a(s) Cor(s)"
                      #selectedTextFormat = "static"
                    ),
                    multiple = T,
                    width = '190px'
                    
                  ),
                  # FILTROS ----------------------------------------------------------------------------------------------------
                  actionBttn(
                    inputId = "reset_input_s",
                    label = HTML("<h4>Limpar Filtros</h4> <span style='font-size:14px'> (Limpe os filtros sempre que realizar nova consulta) </span>"),                                                style = "fill", 
                    color = "primary",size = "xs"
                  ),
                  #actionButton("reset_input_s", "Limpar Filtros",style="position:relative;height:40px;bottom:-1.95rem;",width = "150px")
                )),
        # GRÁFICOS ABA DE SERIE HISTORICA
        fluidRow(
          bs4Card(width = 6,title = div(HTML('Inscritos')),elevation = 4,maximizable = T,status = "danger", solidHeader = T,collapsed = F,headerBorder = F,
                  uiOutput("serie_inscritos")
          ),
          bs4Card(title = div(HTML('Matriculados')),elevation = 4,maximizable = T,status = "danger", solidHeader = T,collapsed = F,headerBorder = F,
                  uiOutput("serie_matriculados"))
          
        )
),
# ABA MAPA LOCAIS DE PROVA
tabItem(tabName = "mapa_prova",
        fluidPage(column(width = 12,div(style = "height:820px;",
                                        leafletOutput("map", width = "100%",height = "100%"),
                                        absolutePanel(top = 20, right = 10,
                                                      selectInput("ano_mapa", "Selecione o Ano:",choices = sort(c(2000:ano_atual),decreasing = T)))
        )))
),
# ABA INFORMACOES GERAIS
tabItem(tabName = "pag1",
        fluidRow(
          bs4Accordion(id = "acord1",width = 6,
                       bs4AccordionItem(title = "Apresentação",solidHeader = T,status = "danger",collapsed = F,
                                        # TEXTO -----------------------------------------------------------------
                                        
                                        p("Esse painel apresenta dados agregados do perfil sócio econômico dos candidatos inscritos e alunos matriculados em todas as formas de ingresso a partir do ano 2000.", style ="font-size: 20px;"),
                                        p("O painel permite gerar estatísticas gerais, por área, curso e por alguns perfis específicos, apresentados a partir de gráficos dinâmicos. " , style ="font-size: 20px;"),
                                        p("Os resultados estão organizados em cinco seções dispostas em abas na parte superior do painel:" , style ="font-size: 20px;"),
                                        p(HTML("<b>- Dicionário:</b>"),"apresenta uma breve descrição das variáveis presentes no banco de dados e algumas siglas utilizadas." , style ="font-size: 20px;"),
                                        p(HTML("<b>- Perfil Socioeconômico:</b>"), "permite a consulta de variáveis do perfil sócio econômico dos inscritos e matriculados. ", style ="font-size: 20px;"),
                                        p(HTML("<b>- Série Histórica: </b>"), "apresenta gráficos com a série histórica, iniciando a partir do ano 2000, de estatísticas relacionadas ao perfil sócio econômico dos inscritos e matriculados.", style ="font-size: 20px;"),
                                        p(HTML("<b>- Inscritos e Suas Formas de Ingresso: apresenta o número de inscritos por tipos de ingresso e suas intersecções (um candidato pode participar de mais de um processo). </b>"), "apresenta o número de inscritos por tipos de ingresso", style ="font-size: 20px;"),
                                        p(HTML("<b>- Mapa Locais de Prova: </b>"),"apresenta um mapa com a localização dos locais de prova e número de inscritos por local.",  style ="font-size: 20px;"),
                                        p("-", style ="font-size: 2px;")
                                        # TEXTO -----------------------------------------------------------------
                       )
          ),
          bs4Accordion(id="acord2",width = 6,
                       bs4AccordionItem(title = "Guia Rápido de Uso",solidHeader = T,status = "danger",collapsed = F,
                                        # TEXTO ------------------------------------------------------------------
                                        p("O presente painel apresenta em cada uma das seções uma coleção de funcionalidades que podem ser exploradas através das abas em vermelho.", style ="font-size: 20px;"),
                                        p("Ao clicar no ícone (+) a aba se expande e no (-) ela minimiza.", style ="font-size: 20px;"),
                                        p("Ao clicar nos ícones de seta é possível expandir a aba em tela cheia.", style ="font-size: 20px;"),
                                        p("As abas que não tiverem tais ícones podem ser expandidas/recolhidas, basta clicar em qualquer região da aba.", style ="font-size: 20px;"),
                                        p("Todos os gráficos apresentados são reativos as opções selecionadas e permitem que o usuário possa:", style ="font-size: 20px;"),
                                        p("- Ao passar o ponteiro do mouse sobre os elementos do gráfico visualizar estatísticas complementares.", style ="font-size: 20px;"),
                                        p("- Selecionar e excluir os diferentes subgrupos apresentados no gráfico.", style ="font-size: 20px;"),
                                        p("- Por meio de menu no canto superior direito das barras horizontais presentes nos gráficos o usuário pode fazer o download do gráfico em diferentes formatos e de uma planilha em formato .csv ou .xlsx  das estatísticas presentes nos gráficos.", style ="font-size: 20px;"),
                                        p("- Ao clicar no gráfico e selecionar uma região com o cursor é aplicado um zoom.", style ="font-size: 20px;"),
                                        p("-", style ="font-size: 2px;")
                                        # TEXTO ------------------------------------------------------------------ 
                       )      
          )
        )
),
# ABA DICIONARIO
tabItem(tabName = "pag2",
        fluidRow(
          bs4Card(title = div(HTML('Dicionário')),elevation = 4,maximizable = T,status = "danger", solidHeader = T,collapsed = F,headerBorder = F,
                  
                  selectInput("variavel_dic",width = "100%",label = HTML("<h5> <b> Selecionar uma variável </b></h5>"),multiple = F,selected = "Cor",
                              choices = opcoes_variavel_ref_perfil),
                  uiOutput("texto_dic")
                  
          ),
          bs4Accordion(id = "acord4",width = 6,
                       bs4AccordionItem(title = "Informação Adicionais ",solidHeader = T,status = "danger",collapsed = F,
                                        # TEXTO ------------------------------------------------------------------
                                        p("Formas de Ingresso", style ="font-size: 22px;", style = "font-weight: bold;"),
                                        
                                        p("Todos os processos de ingresso são independentes e um candidato pode se inscrever em mais de um processo concomitantemente. Por essa razão, quando apresentadas as estatísticas gerais de inscritos, englobando mais de um processo, os candidatos inscritos em mais de um processo foram contados apenas uma vez.", style ="font-size: 20px;"),
                                        p("Em 2021, por causa da pandemia da COVID-19, houve atraso na aplicação das provas do ENEM, impossibilitando o ingresso pela modalidade ENEM-Unicamp, as vagas dessa modalidade passaram para o Vestibular Unicamp.", style ="font-size: 20px;"),
                                        p("-", style ="font-size: 2px;"),
                                        p("Políticas de Inclusão", style ="font-size: 22px;", style = "font-weight: bold;"),
                                        p(HTML("<b>Programa de Ação Afirmativa e Inclusão Social (PAAIS):</b>"),"programa implementado no vestibular de 2005 que bonificava candidatos que estudaram o ensino médio integralmente em escola pública, com uma bonificação extra para aqueles que além de estudarem todo o ensino médio em escolas públicas também se autodeclaravam pretos, pardos ou indígena (PPI).", style ="font-size: 20px;"),
                                        p("Inicialmente a bonificação era de 30 (40) pontos aplicados na nota final do vestibular. Em 2014 a bonificação foi aumentada para 60 (80) pontos. Em 2016 a pontuação passou a ser aplicada também na primeira fase sendo de: 60 (80) pontos a nota final de primeira fase e de 90 (120) pontos a nota final de cada uma das provas de segunda fase.", style ="font-size: 20px;"),
                                        p("Em 2019 com a implementação das cotas étnico raciais o PAAIS passou por uma nova revisão. A bonificação passou a ser de 20 pontos se o candidato cursou os anos finais do ensino fundamental (6º ao 9º ano) integralmente em escolas públicas e 40 pontos se o candidato cursou o ensino médio integralmente em escolas públicas. A pontuação pode ser cumulativa (20 + 40) e é aplicada as notas finais da prova de primeira e de segunda fase.", style ="font-size: 20px;"),
                                        p("-", style ="font-size: 2px;"),
                                        p(HTML("<b>Cotas étnicos raciais:</b>"),"implementado a partir do ingresso 2019 são previsto um piso de 25% das vagas regulares reservadas para candidatos autodeclarados pretos ou pardos, distribuídas da seguinte forma: 15% no Vestibular Unicamp e 10% no Edital ENEM-Unicamp.", style ="font-size: 20px;")
                                        # TEXTO ------------------------------------------------------------------
                       )
          )
        )
),# ABA INSCRITOS E SUAS FORMAS DE INGRESSO
tabItem(tabName = "forma_ingr_insc",
        bootstrapPage(
          tags$style(type = "text/css", "html, body {width:100%;height:800px}"),
          
          highchartOutput("chart", width = "100%", height = "800px"),
          
          absolutePanel(top = 78, right = 78,
                        selectInput("ano_form_ingr", "Selecione o Ano:",
                                    choices = sort(c(2011:ano_atual),decreasing = T)
                        )
          )
        )
)
                                       )
                  ), #RODAPÉ
footer =  dashboardFooter(right  = tags$footer(HTML("© 2022 COMVEST - Comissão Permanente para os Vestibulares da UNICAMP"),align = "right",style="display: flex;height: 40px;", imageOutput("image",height = "40px")))

)
##### ------------------------------------------------------------------------------------------------------------



server = function(input,output,session){
  waiter_hide()
  
  
  # FILTROS - PERFIL
  filtro_ano        <- reactive({if(is.null(input$ano))       levels(dados[,Ano]) else input$ano})
  filtro_area       <- reactive({if(is.null(input$area))      levels(dados[,area_insc]) else input$area})
  filtro_cur_insc   <- reactive({if(is.null(input$cur_insc))  levels(dados[,cur_insc]) else input$cur_insc})
  filtro_processo   <- reactive({if(is.null(input$processo))  levels(dados[,Processo]) else input$processo})
  filtro_forma_ingr <- reactive({if(is.null(input$forma_ingr))levels(dados[,Forma_Ingresso]) else input$forma_ingr})
  filtro_isento     <- reactive({if(is.null(input$isento))    levels(dados[,Isento]) else input$isento})
  filtro_escola     <- reactive({if(is.null(input$escola))    levels(dados[,Ens_med]) else input$escola})
  #filtro_escola     <- reactive({if(is.null(input$escola))    levels(dados[,Ens_med]) else if(input$escola == "Outro") c("Em branco","Outro","Público/Privado") else input$escola })
  filtro_sexo       <- reactive({if(is.null(input$sexo))      levels(dados[,Sexo]) else input$sexo})
  filtro_cor        <- reactive({if(is.null(input$cor))       levels(dados[,Cor]) else input$cor})
  filtro_mat        <- reactive({if(is.null(input$cur_insc))  levels(dados[,cur_mat]) else input$cur_insc})
  
  # DADOS FILTRADOS - PERFIL
  dados_reativo <- reactive({
    
    dados[
      Ano             %in% filtro_ano()        &
        #area_insc      %in% filtro_area()       &
        #cur_insc       %in% filtro_cur_insc()   &
        Processo        %in% filtro_processo()   &
        Forma_Ingresso  %in% filtro_forma_ingr() &
        Isento          %in% filtro_isento()     &
        Ens_med         %in% filtro_escola()     &
        Sexo            %in% filtro_sexo()       &
        Cor             %in% filtro_cor()
      ,]
    
  })
  
  
  # FILTROS INTERLIGADOS - PERFIL
  observeEvent(input$ano,{
    updatePickerInput(session,inputId = "area",choices = levels(dados_reativo()$area_insc)[unique(dados_reativo()$area_insc)] %>% sort(),selected = input$area)
    updatePickerInput(session,inputId = "cur_insc",choices = list(
      "Artes" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Artes")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ano %in% input$ano & dados_reativo()$area_insc == "Artes")])] %>% sort(),
      "Ciências Biológicas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ano %in% input$ano & dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])] %>% sort(),
      "Ciências Exatas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Exatas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ano %in% input$ano & dados_reativo()$area_insc == "Ciências Exatas")])] %>% sort(),
      "Ciências Humanas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Humanas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ano %in% input$ano & dados_reativo()$area_insc == "Ciências Humanas")])] %>% sort(),
      "Engenharias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Engenharias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ano %in% input$ano & dados_reativo()$area_insc == "Engenharias")])] %>% sort(),
      "Tecnológias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Tecnológias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ano %in% input$ano & dados_reativo()$area_insc == "Tecnológias")])] %>% sort(),
      "Profis" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Profis")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ano %in% input$ano & dados_reativo()$area_insc == "Profis")])] %>% sort()
      
    ),selected = input$cur_insc)
    #levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Ano %in% input$ano)])] %>% sort(),selected = input$cur_insc)
    updatePickerInput(session,inputId = "processo",choices =  levels(dados_reativo()$Processo)[unique(dados_reativo()$Processo)]%>% sort(),selected = input$processo)
    #updatePickerInput(session,inputId = "forma_ingr",choices = levels(dados$Forma_Ingresso)[unique(dados$Forma_Ingresso[which(dados$Ano %in% input$ano)])] %>% sort(),selected = input$forma_ingr)
    updatePickerInput(session,inputId = "forma_ingr",choices = list(
      "Vestibular Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Unicamp" )])] %>% sort(),
      "ENEM-Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "ENEM-Unicamp" )])] %>% sort(),
      "Vestibular Indígena" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Indígena" )])] %>% sort(),
      "Vagas Olímpicas" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vagas Olímpicas" )])] %>% sort(),
      "ProFIS" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo %in% c("ProFIS","Egresso ProFIS"))])] %>% sort()
    ),selected = input$forma_ingr)
    updatePickerInput(session,inputId = "isento",choices = levels(dados_reativo()$Isento)[unique(dados_reativo()$Isento)] %>% sort(),selected = input$isento)
    updatePickerInput(session,inputId = "escola",choices = levels(dados_reativo()$Ens_med)[unique(dados_reativo()$Ens_med)] %>% sort(),selected = input$escola)
    updatePickerInput(session,inputId = "sexo",choices = levels(dados_reativo()$Sexo)[unique(dados_reativo()$Sexo)] %>% sort(),selected = input$sexo)
    updatePickerInput(session,inputId = "cor",choices =levels(dados_reativo()$Cor)[unique(dados_reativo()$Cor)] %>% sort(),selected = input$cor) 
  })
  
  observeEvent(input$area,{
    #updatePickerInput(session,inputId = "ano",choices = levels(dados_reativo()$Ano)[unique(dados$Ano[which(dados$area_insc %in% input$area)])] ,selected = input$ano)
    updatePickerInput(session,inputId = "cur_insc",choices = levels(dados_reativo()$cur_insc)[unique(dados_reativo()$cur_insc[which(dados_reativo()$area_insc %in% input$area)])]%>% sort(),selected = input$cur_insc)
    updatePickerInput(session,inputId = "processo",choices = levels(dados_reativo()$Processo)[unique(dados_reativo()$Processo)]%>% sort(),selected = input$processo)
    #updatePickerInput(session,inputId = "forma_ingr",choices = levels(dados$Forma_Ingresso)[unique(dados$Forma_Ingresso[which(dados$area_insc %in% input$area)])]%>% sort(),selected = input$forma_ingr)
    updatePickerInput(session,inputId = "forma_ingr",choices = list(
      "Vestibular Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Unicamp" )])] %>% sort(),
      "ENEM-Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "ENEM-Unicamp" )])] %>% sort(),
      "Vestibular Indígena" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Indígena" )])] %>% sort(),
      "Vagas Olímpicas" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vagas Olímpicas" )])] %>% sort(),
      "ProFIS" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo %in% c("ProFIS","Egresso ProFIS"))])] %>% sort()
    ),selected = input$forma_ingr)
    updatePickerInput(session,inputId = "isento",choices =  levels(dados_reativo()$Isento)[unique(dados_reativo()$Isento)]%>% sort(),selected = input$isento)
    updatePickerInput(session,inputId = "escola",choices =  levels(dados_reativo()$Ens_med)[unique(dados_reativo()$Ens_med)]%>% sort(),selected = input$escola)
    updatePickerInput(session,inputId = "sexo",choices = levels(dados_reativo()$Sexo)[unique(dados_reativo()$Sexo)]%>% sort(),selected = input$sexo)
    updatePickerInput(session,inputId = "cor",choices =  levels(dados_reativo()$Cor)[unique(dados_reativo()$Cor)]%>% sort(),selected = input$cor) 
  })
  
  observeEvent(input$cur_insc,{
    #updatePickerInput(session,inputId = "ano",choices = levels(dados$Ano)[unique(dados$Ano[which(dados$cur_insc %in% input$cur_insc)])] ,selected = input$ano)
    updatePickerInput(session,inputId = "area",choices = levels(dados_reativo()$area_insc)[unique(dados_reativo()$area_insc[which(dados_reativo()$cur_insc %in% input$cur_insc)])] %>% sort(),selected = input$area)
    updatePickerInput(session,inputId = "processo",choices = levels(dados_reativo()$Processo)[unique(dados_reativo()$Processo)] %>% sort(),selected = input$processo)
    updatePickerInput(session,inputId = "forma_ingr",choices = list(
      "Vestibular Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Unicamp" )])] %>% sort(),
      "ENEM-Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "ENEM-Unicamp" )])] %>% sort(),
      "Vestibular Indígena" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Indígena" )])] %>% sort(),
      "Vagas Olímpicas" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vagas Olímpicas" )])] %>% sort(),
      "ProFIS" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo %in% c("ProFIS","Egresso ProFIS"))])] %>% sort()
    ),selected = input$forma_ingr)
    #updatePickerInput(session,inputId = "forma_ingr",choices = levels(dados$Forma_Ingresso)[unique(dados$Forma_Ingresso[which(dados$cur_insc %in% input$cur_insc)])] %>% sort(),selected = input$forma_ingr)
    updatePickerInput(session,inputId = "isento",choices = levels(dados_reativo()$Isento)[unique(dados_reativo()$Isento)] %>% sort(),selected = input$isento)
    updatePickerInput(session,inputId = "escola",choices =levels(dados_reativo()$Ens_med)[unique(dados_reativo()$Ens_med)] %>% sort(),selected = input$escola)
    updatePickerInput(session,inputId = "sexo",choices =levels(dados_reativo()$Sexo)[unique(dados_reativo()$Sexo)] %>% sort(),selected = input$sexo)
    updatePickerInput(session,inputId = "cor",choices = levels(dados_reativo()$Cor)[unique(dados_reativo()$Cor)] %>% sort(),selected = input$cor) 
    
  })
  
  observeEvent(input$processo,{
    #updatePickerInput(session,inputId = "ano",choices = levels(dados$Ano)[unique(dados$Ano[which(dados$Processo %in% input$processo)])],selected = input$ano)
    updatePickerInput(session,inputId = "area",choices = levels(dados_reativo()$area_insc)[unique(dados_reativo()$area_insc[which(dados_reativo()$Processo %in% input$processo)])]%>% sort(),selected = input$area)
    #updatePickerInput(session,inputId = "cur_insc",choices = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Processo %in% input$processo)])]%>% sort(),selected = input$cur_insc)
    updatePickerInput(session,inputId = "cur_insc",choices = list(
      "Artes" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Artes")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Processo %in% input$processo & dados_reativo()$area_insc == "Artes")])] %>% sort(),
      "Ciências Biológicas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Processo %in% input$processo & dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])] %>% sort(),
      "Ciências Exatas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Exatas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Processo %in% input$processo & dados_reativo()$area_insc == "Ciências Exatas")])] %>% sort(),
      "Ciências Humanas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Humanas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Processo %in% input$processo & dados_reativo()$area_insc == "Ciências Humanas")])] %>% sort(),
      "Engenharias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Engenharias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Processo %in% input$processo & dados_reativo()$area_insc == "Engenharias")])] %>% sort(),
      "Tecnológias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Tecnológias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Processo %in% input$processo & dados_reativo()$area_insc == "Tecnológias")])] %>% sort(),
      "Profis" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Profis")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Processo %in% input$processo & dados_reativo()$area_insc == "Profis")])] %>% sort()
      
    ),selected = input$cur_insc)
    updatePickerInput(session,inputId = "forma_ingr",choices = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso)]%>% sort(),selected = input$forma_ingr)
    updatePickerInput(session,inputId = "isento",choices = levels(dados_reativo()$Isento)[unique(dados_reativo()$Isento)]%>% sort(),selected = input$isento)
    updatePickerInput(session,inputId = "escola",choices = levels(dados_reativo()$Ens_med)[unique(dados_reativo()$Ens_med)]%>% sort(),selected = input$escola)
    updatePickerInput(session,inputId = "sexo",choices =levels(dados_reativo()$Sexo)[unique(dados_reativo()$Sexo)]%>% sort(),selected = input$sexo)
    updatePickerInput(session,inputId = "cor",choices = levels(dados_reativo()$Cor)[unique(dados_reativo()$Cor)]%>% sort(),selected = input$cor) 
  })
  
  observeEvent(input$forma_ingr,{
    #updatePickerInput(session,inputId = "ano",choices = levels(dados$Ano)[unique(dados$Ano[which(dados$Forma_Ingresso %in% input$forma_ingr)])],selected = input$ano)
    updatePickerInput(session,inputId = "area",choices = levels(dados_reativo()$area_insc)[unique(dados_reativo()$area_insc[which(dados_reativo()$Forma_Ingresso %in% input$forma_ingr)])]%>% sort(),selected = input$area)
    #updatePickerInput(session,inputId = "cur_insc",choices = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Forma_Ingresso %in% input$forma_ingr)])]%>% sort(),selected = input$cur_insc)
    updatePickerInput(session,inputId = "cur_insc",choices = list(
      "Artes" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Artes")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Forma_Ingresso %in% input$forma_ingr & dados_reativo()$area_insc == "Artes")])] %>% sort(),
      "Ciências Biológicas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Forma_Ingresso %in% input$forma_ingr & dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])] %>% sort(),
      "Ciências Exatas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Exatas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Forma_Ingresso %in% input$forma_ingr & dados_reativo()$area_insc == "Ciências Exatas")])] %>% sort(),
      "Ciências Humanas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Humanas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Forma_Ingresso %in% input$forma_ingr & dados_reativo()$area_insc == "Ciências Humanas")])] %>% sort(),
      "Engenharias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Engenharias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Forma_Ingresso %in% input$forma_ingr & dados_reativo()$area_insc == "Engenharias")])] %>% sort(),
      "Tecnológias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Tecnológias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Forma_Ingresso %in% input$forma_ingr & dados_reativo()$area_insc == "Tecnológias")])] %>% sort(),
      "Profis" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Profis")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Forma_Ingresso %in% input$forma_ingr & dados_reativo()$area_insc == "Profis")])] %>% sort()
      
    ),selected = input$cur_insc)
    updatePickerInput(session,inputId = "processo",choices = levels(dados_reativo()$Processo)[unique(dados_reativo()$Processo)]%>% sort(),selected = input$processo)
    updatePickerInput(session,inputId = "isento",choices = levels(dados_reativo()$Isento)[unique(dados_reativo()$Isento)]%>% sort(),selected = input$isento)
    updatePickerInput(session,inputId = "escola",choices = levels(dados_reativo()$Ens_med)[unique(dados_reativo()$Ens_med)]%>% sort(),selected = input$escola)
    updatePickerInput(session,inputId = "sexo",choices = levels(dados_reativo()$Sexo)[unique(dados_reativo()$Sexo)]%>% sort(),selected = input$sexo)
    updatePickerInput(session,inputId = "cor",choices = levels(dados_reativo()$Cor)[unique(dados_reativo()$Cor)]%>% sort(),selected = input$cor) 
  })
  
  observeEvent(input$isento,{
    #updatePickerInput(session,inputId = "ano",choices = levels(dados$Ano)[unique(dados$Ano[which(dados$Isento %in% input$isento)])],selected = input$ano)
    updatePickerInput(session,inputId = "area",choices = levels(dados_reativo()$area_insc)[unique(dados_reativo()$area_insc[which(dados_reativo()$Isento %in% input$isento)])]%>% sort(),selected = input$area)
    updatePickerInput(session,inputId = "cur_insc",choices = list(
      "Artes" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Artes")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Isento %in% input$isento & dados_reativo()$area_insc == "Artes")])] %>% sort(),
      "Ciências Biológicas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Isento %in% input$isento & dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])] %>% sort(),
      "Ciências Exatas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Exatas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Isento %in% input$isento & dados_reativo()$area_insc == "Ciências Exatas")])] %>% sort(),
      "Ciências Humanas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Humanas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Isento %in% input$isento & dados_reativo()$area_insc == "Ciências Humanas")])] %>% sort(),
      "Engenharias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Engenharias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Isento %in% input$isento & dados_reativo()$area_insc == "Engenharias")])] %>% sort(),
      "Tecnológias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Tecnológias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Isento %in% input$isento & dados_reativo()$area_insc == "Tecnológias")])] %>% sort(),
      "Profis" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Profis")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Isento %in% input$isento & dados_reativo()$area_insc == "Profis")])] %>% sort()
      
    ),selected = input$cur_insc)
    #updatePickerInput(session,inputId = "cur_insc",choices = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Isento %in% input$isento)])]%>% sort(),selected = input$cur_insc)
    updatePickerInput(session,inputId = "processo",choices = levels(dados_reativo()$Processo)[unique(dados_reativo()$Processo)]%>% sort(),selected = input$processo)
    updatePickerInput(session,inputId = "forma_ingr",choices = list(
      "Vestibular Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Unicamp" )])] %>% sort(),
      "ENEM-Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "ENEM-Unicamp" )])] %>% sort(),
      "Vestibular Indígena" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Indígena" )])] %>% sort(),
      "Vagas Olímpicas" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vagas Olímpicas" )])] %>% sort(),
      "ProFIS" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo %in% c("ProFIS","Egresso ProFIS"))])] %>% sort()
    ),selected = input$forma_ingr)
    #updatePickerInput(session,inputId = "forma_ingr",choices =  levels(dados$Forma_Ingresso)[unique(dados$Forma_Ingresso[which(dados$Isento %in% input$isento)])]%>% sort(),selected = input$forma_ingr)
    updatePickerInput(session,inputId = "escola",choices = levels(dados_reativo()$Ens_med)[unique(dados_reativo()$Ens_med)]%>% sort(),selected = input$escola)
    updatePickerInput(session,inputId = "sexo",choices = levels(dados_reativo()$Sexo)[unique(dados_reativo()$Sexo)]%>% sort(),selected = input$sexo)
    updatePickerInput(session,inputId = "cor",choices =  levels(dados_reativo()$Cor)[unique(dados_reativo()$Cor)]%>% sort(),selected = input$cor) 
    
  })
  
  observeEvent(input$escola,{
    #updatePickerInput(session,inputId = "ano",choices = levels(dados$Ano)[unique(dados$Ano[which(dados$Ens_med %in% input$escola)])],selected = input$ano)
    updatePickerInput(session,inputId = "area",choices = levels(dados_reativo()$area_insc)[unique(dados_reativo()$area_insc[which(dados_reativo()$Ens_med %in% input$escola)])]%>% sort(),selected = input$area)
    #updatePickerInput(session,inputId = "cur_insc",choices = levels(dados_reativo()$cur_insc)[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ens_med %in% input$escola)])]%>% sort(),selected = input$cur_insc)
    updatePickerInput(session,inputId = "cur_insc",choices = list(
      "Artes" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Artes")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ens_med %in% input$escola & dados_reativo()$area_insc == "Artes")])] %>% sort(),
      "Ciências Biológicas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ens_med %in% input$escola & dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])] %>% sort(),
      "Ciências Exatas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Exatas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ens_med %in% input$escola & dados_reativo()$area_insc == "Ciências Exatas")])] %>% sort(),
      "Ciências Humanas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Humanas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ens_med %in% input$escola & dados_reativo()$area_insc == "Ciências Humanas")])] %>% sort(),
      "Engenharias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Engenharias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ens_med %in% input$escola & dados_reativo()$area_insc == "Engenharias")])] %>% sort(),
      "Tecnológias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Tecnológias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ens_med %in% input$escola & dados_reativo()$area_insc == "Tecnológias")])] %>% sort(),
      "Profis" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Profis")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Ens_med %in% input$escola & dados_reativo()$area_insc == "Profis")])] %>% sort()
      
    ),selected = input$cur_insc)
    updatePickerInput(session,inputId = "processo",choices = levels(dados_reativo()$Processo)[unique(dados_reativo()$Processo)]%>% sort(),selected = input$processo)
    updatePickerInput(session,inputId = "forma_ingr",choices = list(
      "Vestibular Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Unicamp" )])] %>% sort(),
      "ENEM-Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "ENEM-Unicamp" )])] %>% sort(),
      "Vestibular Indígena" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Indígena" )])] %>% sort(),
      "Vagas Olímpicas" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vagas Olímpicas" )])] %>% sort(),
      "ProFIS" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo %in% c("ProFIS","Egresso ProFIS"))])] %>% sort()
    ),selected = input$forma_ingr)
    #updatePickerInput(session,inputId = "forma_ingr",choices = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso)]%>% sort(),selected = input$forma_ingr)
    updatePickerInput(session,inputId = "isento",choices = levels(dados_reativo()$Isento)[unique(dados_reativo()$Isento)]%>% sort(),selected = input$isento)
    updatePickerInput(session,inputId = "sexo",choices = levels(dados_reativo()$Sexo)[unique(dados_reativo()$Sexo)]%>% sort(),selected = input$sexo)
    updatePickerInput(session,inputId = "cor",choices = levels(dados_reativo()$Cor)[unique(dados_reativo()$Cor)]%>% sort(),selected = input$cor) 
  })
  
  observeEvent(input$sexo,{
    #updatePickerInput(session,inputId = "ano",choices = levels(dados_reativo()$Ano)[unique(dados_reativo()$Ano[which(dados_reativo()$Sexo %in% input$sexo)])],selected = input$ano)
    updatePickerInput(session,inputId = "area",choices = levels(dados_reativo()$area_insc)[unique(dados_reativo()$area_insc[which(dados_reativo()$Sexo %in% input$sexo)])]%>% sort(),selected = input$area)
    #updatePickerInput(session,inputId = "cur_insc",choices = levels(dados_reativo()$cur_insc)[unique(dados_reativo()$cur_insc)]%>% sort(),selected = input$cur_insc)
    updatePickerInput(session,inputId = "cur_insc",choices = list(
      "Artes" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Artes")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Sexo %in% input$sexo & dados_reativo()$area_insc == "Artes")])] %>% sort(),
      "Ciências Biológicas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Sexo %in% input$sexo & dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])] %>% sort(),
      "Ciências Exatas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Exatas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Sexo %in% input$sexo & dados_reativo()$area_insc == "Ciências Exatas")])] %>% sort(),
      "Ciências Humanas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Humanas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Sexo %in% input$sexo & dados_reativo()$area_insc == "Ciências Humanas")])] %>% sort(),
      "Engenharias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Engenharias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Sexo %in% input$sexo & dados_reativo()$area_insc == "Engenharias")])] %>% sort(),
      "Tecnológias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Tecnológias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Tecnológias")])] %>% sort(),
      "Profis" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Profis")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Sexo %in% input$sexo & dados_reativo()$area_insc == "Profis")])] %>% sort()
      
    ),selected = input$cur_insc)
    updatePickerInput(session,inputId = "processo",choices =levels(dados_reativo()$Processo)[unique(dados_reativo()$Processo)]%>% sort(),selected = input$processo)
    updatePickerInput(session,inputId = "forma_ingr",choices = list(
      "Vestibular Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Unicamp" )])] %>% sort(),
      "ENEM-Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "ENEM-Unicamp" )])] %>% sort(),
      "Vestibular Indígena" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Indígena" )])] %>% sort(),
      "Vagas Olímpicas" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vagas Olímpicas" )])] %>% sort(),
      "ProFIS" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo %in% c("ProFIS","Egresso ProFIS"))])] %>% sort()
    ),selected = input$forma_ingr)
    #updatePickerInput(session,inputId = "forma_ingr",choices = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso)]%>% sort(),selected = input$forma_ingr)
    updatePickerInput(session,inputId = "isento",choices = levels(dados_reativo()$Isento)[unique(dados_reativo()$Isento)]%>% sort(),selected = input$isento)
    updatePickerInput(session,inputId = "escola",choices =  levels(dados_reativo()$Ens_med)[unique(dados_reativo()$Ens_med)]%>% sort(),selected = input$escola)
    updatePickerInput(session,inputId = "cor",choices =  levels(dados_reativo()$Cor)[unique(dados_reativo()$Cor)]%>% sort(),selected = input$cor) 
  })
  
  observeEvent(input$cor,{
    #updatePickerInput(session,inputId = "ano",choices = levels(dados_reativo()$Ano)[unique(dados_reativo()$Ano[which(dados_reativo()$Cor %in% input$cor)])],selected = input$ano)
    updatePickerInput(session,inputId = "area",choices = levels(dados_reativo()$area_insc)[unique(dados_reativo()$area_insc[which(dados_reativo()$Cor %in% input$cor)])]%>% sort(),selected = input$area)
    #updatePickerInput(session,inputId = "cur_insc",choices = levels(dados_reativo()$cur_insc)[unique(dados_reativo()$cur_insc)]%>% sort(),selected = input$cur_insc)
    updatePickerInput(session,inputId = "cur_insc",choices = list(
      "Artes" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Artes")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Cor %in% input$cor & dados_reativo()$area_insc == "Artes")])] %>% sort(),
      "Ciências Biológicas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Cor %in% input$cor & dados_reativo()$area_insc %in% c("Ciências Biológicas","Medicina"))])] %>% sort(),
      "Ciências Exatas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Exatas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Cor %in% input$cor & dados_reativo()$area_insc == "Ciências Exatas")])] %>% sort(),
      "Ciências Humanas" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Ciências Humanas")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Cor %in% input$cor & dados_reativo()$area_insc == "Ciências Humanas")])] %>% sort(),
      "Engenharias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Engenharias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Cor %in% input$cor & dados_reativo()$area_insc == "Engenharias")])] %>% sort(),
      "Tecnológias" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Tecnológias")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Cor %in% input$cor & dados_reativo()$area_insc == "Tecnológias")])] %>% sort(),
      "Profis" = levels(dados_reativo()$cur_insc[which(dados_reativo()$area_insc == "Profis")])[unique(dados_reativo()$cur_insc[which(dados_reativo()$Cor %in% input$cor & dados_reativo()$area_insc == "Profis")])] %>% sort()
      
    ),selected = input$cur_insc)
    updatePickerInput(session,inputId = "processo",choices = levels(dados_reativo()$Processo)[unique(dados_reativo()$Processo)]%>% sort(),selected = input$processo)
    updatePickerInput(session,inputId = "forma_ingr",choices = list(
      "Vestibular Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Unicamp" )])] %>% sort(),
      "ENEM-Unicamp" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "ENEM-Unicamp" )])] %>% sort(),
      "Vestibular Indígena" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vestibular Indígena" )])] %>% sort(),
      "Vagas Olímpicas" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo == "Vagas Olímpicas" )])] %>% sort(),
      "ProFIS" = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso[which(dados_reativo()$Processo %in% c("ProFIS","Egresso ProFIS"))])] %>% sort()
    ),selected = input$forma_ingr)
    #updatePickerInput(session,inputId = "forma_ingr",choices = levels(dados_reativo()$Forma_Ingresso)[unique(dados_reativo()$Forma_Ingresso)]%>% sort(),selected = input$forma_ingr)
    updatePickerInput(session,inputId = "isento",choices = levels(dados_reativo()$Isento)[unique(dados_reativo()$Isento)]%>% sort(),selected = input$isento)
    updatePickerInput(session,inputId = "escola",choices = levels(dados_reativo()$Ens_med)[unique(dados_reativo()$Ens_med)]%>% sort(),selected = input$escola)
    updatePickerInput(session,inputId = "sexo",choices = levels(dados_reativo()$Sexo)[unique(dados_reativo()$Sexo)]%>% sort(),selected = input$sexo)
  }) 
  
  
  
  
  # RESET INPUT - PERFIL
  observe({
    req(input$reset_input)
    updatePickerInput(session,inputId = "ano",choices = sort(levels(dados[,Ano]),decreasing = T),selected = ano_atual)
    updatePickerInput(session,inputId = "area",choices = levels(dados$area_insc),selected = NULL)
    updatePickerInput(session,inputId = "cur_insc",choices = list(
      "Artes" = levels(dados$cur_insc[which(dados$area_insc == "Artes")])[unique(dados$cur_insc[which(dados$Ano == ano_atual & dados$area_insc == "Artes")])] %>% sort(),
      "Ciências Biológicas" = levels(dados$cur_insc[which(dados$area_insc %in% c("Ciências Biológicas","Medicina"))])[unique(dados$cur_insc[which(dados$Ano == ano_atual & dados$area_insc %in% c("Ciências Biológicas","Medicina"))])] %>% sort(),
      "Ciências Exatas" = levels(dados$cur_insc[which(dados$area_insc == "Ciências Exatas")])[unique(dados$cur_insc[which(dados$Ano == ano_atual & dados$area_insc == "Ciências Exatas")])] %>% sort(),
      "Ciências Humanas" = levels(dados$cur_insc[which(dados$area_insc == "Ciências Humanas")])[unique(dados$cur_insc[which(dados$Ano == ano_atual & dados$area_insc == "Ciências Humanas")])] %>% sort(),
      "Engenharias" = levels(dados$cur_insc[which(dados$area_insc == "Engenharias")])[unique(dados$cur_insc[which(dados$Ano == ano_atual & dados$area_insc == "Engenharias")])] %>% sort(),
      "Tecnológias" = levels(dados$cur_insc[which(dados$area_insc == "Tecnológias")])[unique(dados$cur_insc[which(dados$Ano == ano_atual & dados$area_insc == "Tecnológias")])] %>% sort(),
      "Profis" = levels(dados$cur_insc[which(dados$area_insc == "Profis")])[unique(dados$cur_insc[which(dados$Ano == ano_atual & dados$area_insc == "Profis")])] %>% sort()
    ),selected =NULL)
    updatePickerInput(session,inputId = "processo",choices = levels(dados$Processo),selected = NULL)
    updatePickerInput(session,inputId = "forma_ingr",choices = list(
      "Vestibular Unicamp" = levels(dados$Forma_Ingresso)[unique(dados$Forma_Ingresso[which(dados$Ano == ano_atual & dados$Processo == "Vestibular Unicamp" )])] %>% sort(),
      "ENEM-Unicamp" = levels(dados$Forma_Ingresso)[unique(dados$Forma_Ingresso[which(dados$Ano == ano_atual & dados$Processo == "ENEM-Unicamp" )])] %>% sort(),
      "Vestibular Indígena" = levels(dados$Forma_Ingresso)[unique(dados$Forma_Ingresso[which(dados$Ano == ano_atual & dados$Processo == "Vestibular Indígena" )])] %>% sort(),
      "Vagas Olímpicas" = levels(dados$Forma_Ingresso)[unique(dados$Forma_Ingresso[which(dados$Ano == ano_atual & dados$Processo == "Vagas Olímpicas" )])] %>% sort(),
      "ProFIS" = levels(dados$Forma_Ingresso)[unique(dados$Forma_Ingresso[which(dados$Ano == ano_atual & dados$Processo %in% c("ProFIS","Egresso ProFIS"))])] %>% sort()
    ),selected = NULL)
    updatePickerInput(session,inputId = "isento",choices = levels(dados$Isento),selected =NULL)
    updatePickerInput(session,inputId = "escola",choices = levels(dados$Ens_med),selected = NULL)
    updatePickerInput(session,inputId = "sexo",choices = levels(dados$Sexo),selected = NULL)
    updatePickerInput(session,inputId = "cor",choices = levels(dados$Cor),selected =NULL)
  })
  
  # INPUT CRUZAMENTO - PERFIL
  observe({ 
    input$variavel_perfil
    updateSelectInput(session,inputId = "cruzamento_perfil", choices = c("Nenhuma",opcoes_variavel_ref_perfil[! opcoes_variavel_ref_perfil %in% input$variavel_perfil]))
  })
  
  # CAIXAS - PERFIL
  output$p_isento   <- renderbs4ValueBox({
    
    dados_p_isento = cbind(unique(dados_reativo(), by = c("Ano", "ID"))[area_insc %in% filtro_area() & cur_insc %in% filtro_cur_insc(), .(Isento)][, .(n = .N), keyby = .(Isento)][, `:=`(prop = 100 *(n/sum(n)))][Isento == "Isento", .(prop)],dados_reativo()[Candidato == "Matriculado" & cur_mat %in% filtro_cur_insc() & area_mat %in% filtro_area() , .(Isento)][, .(n = .N), keyby = .(Isento)][, `:=`(prop = 100 *(n/sum(n)))][Isento == "Isento", .(prop)] )
    
    
    valor = 
      if(nrow(dados_p_isento)== 1 & sum(is.na(dados_p_isento)) == 0){
        dados_p_isento[,round(.SD,1),.SDcols = 1:2]
      } else if(nrow(dados_p_isento)== 1 & sum(is.na(dados_p_isento)) == 1){
        dados_p_isento[,round(.SD,1),.SDcols = 1][,round(.SD,1),.SDcols = 1][,propp := 0]
      } else 
        data.table(0,0)
    
    #if(nrow(dados_p_isento)== 1){
    #  dados_p_isento[,round(.SD,1),.SDcols = 1:2]
    #} else data.table(0,0)
    
    bs4ValueBox(color = "danger",value = tags$p(style = "font-size: 25px;", "Isentos"),gradient = T,
                subtitle = tags$p(style = "font-size: 20px", "Inscritos: ",
                                  valor[,1],"%",HTML("<br>"),"Matriculados: ",valor[,2],"%"),icon = icon('user-friends'))
  })
  
  output$p_pppi     <- renderbs4ValueBox({
    
    dados_p_pppi = cbind(unique( unique(dados_reativo(), by = c("Ano", "ID"))[area_insc %in% filtro_area() & cur_insc %in% filtro_cur_insc(), .(Cor)][, .(n = .N), keyby = .(Cor)][, `:=`(prop = 100 *(n/sum(n)))][Cor %in% c("Preta", "Parda")][, `:=`(prop = sum(prop))][,.(prop)]),unique(dados_reativo()[Candidato == "Matriculado" & cur_mat %in% filtro_cur_insc() & area_mat %in% filtro_area() , .(Cor)][, .(n = .N),keyby = .(Cor)][, `:=`(prop = 100 * (n/sum(n)))][Cor %in% c("Preta", "Parda")][, `:=`(prop = sum(prop))][, .(prop)]))
    
    valor = 
      if(nrow(dados_p_pppi)== 1 & sum(is.na(dados_p_pppi)) == 0){
        dados_p_pppi[,round(.SD,1),.SDcols = 1:2]
      } else if(nrow(dados_p_pppi)== 1 & sum(is.na(dados_p_pppi)) == 1){
        dados_p_pppi[,round(.SD,1),.SDcols = 1][,round(.SD,1),.SDcols = 1][,propp := 0]
      } else 
        data.table(0,0)
    
    #if(nrow(dados_p_pppi)== 1){
    #  dados_p_pppi[,round(.SD,1),.SDcols = 1:2]
    #} else data.table(0,0)
    
    bs4ValueBox(color = "danger",value = tags$p(style = "font-size: 25px;", "Pretos e Pardos"),
                subtitle = tags$p(style = "font-size: 20px", "Inscritos: ",
                                  valor[,1],"%",HTML("<br>"),"Matriculados: ",valor[,2],"%"
                ),icon = icon('user-friends')
    )
  })
  
  output$p_feminino <- renderbs4ValueBox({
    
    dados_p_feminino = cbind( unique(dados_reativo(), by = c("Ano", "ID"))[area_insc %in% filtro_area() & cur_insc %in% filtro_cur_insc(), .(Sexo)][, .(n = .N), keyby = .(Sexo)][, `:=`(prop = 100*(n/sum(n)))][Sexo == "Feminino", .(prop)],dados_reativo()[Candidato == "Matriculado" & cur_mat %in% filtro_cur_insc() & area_mat %in% filtro_area() , .(Sexo)][, .(n = .N), keyby = .(Sexo)][,`:=`(prop = 100 * (n/sum(n)))][Sexo == "Feminino", .(prop)]
    )
    
    valor = 
      if(nrow(dados_p_feminino)== 1 & sum(is.na(dados_p_feminino)) == 0){
        dados_p_feminino[,round(.SD,1),.SDcols = 1:2]
      } else if(nrow(dados_p_feminino)== 1 & sum(is.na(dados_p_feminino)) == 1){
        dados_p_feminino[,round(.SD,1),.SDcols = 1][,round(.SD,1),.SDcols = 1][,propp := 0]
      } else 
        data.table(0,0)
    
    
    #if(nrow(dados_p_feminino)==1){
    # dados_p_feminino[,round(.SD,1),.SDcols = 1:2]
    #} else data.table(0,0)
    
    bs4ValueBox(color = "danger",value = tags$p(style = "font-size: 25px;", "Sexo Feminino"),gradient = T, subtitle = tags$p(style = "font-size: 20px", "Inscritos: ",
                                                                                                                             valor[,1],"%",HTML("<br>"),"Matriculados: ",valor[,2],"%"
    ),icon = icon('venus'))
  })
  
  output$p_escpub   <- renderbs4ValueBox({
    
    dados_p_escpub = cbind( unique(dados_reativo(), by = c("Ano", "ID"))[area_insc %in% filtro_area() & cur_insc %in% filtro_cur_insc(), .(Ens_med)][, .(n = .N), keyby = .(Ens_med)][, `:=`(prop = 100 *(n/sum(n)))][Ens_med == "Público", .(prop)],dados_reativo()[Candidato == "Matriculado" & cur_mat %in% filtro_cur_insc() & area_mat %in% filtro_area() , .(Ens_med)][, .(n = .N), keyby = .(Ens_med)][, `:=`(prop = 100 * (n/sum(n)))][Ens_med == "Público", .(prop)])
    
    valor = 
      if(nrow(dados_p_escpub)== 1 & sum(is.na(dados_p_escpub)) == 0){
        dados_p_escpub[,round(.SD,1),.SDcols = 1:2]
      } else if(nrow(dados_p_escpub)== 1 & sum(is.na(dados_p_escpub)) == 1){
        dados_p_escpub[,round(.SD,1),.SDcols = 1][,round(.SD,1),.SDcols = 1][,propp := 0]
      } else 
        data.table(0,0)
    
    #if(nrow(dados_p_escpub)==1){
    #  dados_p_escpub[,round(.SD,1),.SDcols = 1:2]
    #} else data.table(0,0)
    
    bs4ValueBox(color = "danger",value = tags$p(style = "font-size: 25px;", "Ensino Médio Público"),gradient = T,
                subtitle = tags$p(style = "font-size: 20px", "Inscritos: ",
                                  valor[,1],"%",HTML("<br>"),"Matriculados: ",valor[,2],"%"
                ),icon = icon('university'))
  })
  
  output$p_tecnico  <- renderbs4ValueBox({
    
    dados_p_tecnico = cbind( unique(dados_reativo(), by = c("Ano", "ID"))[area_insc %in% filtro_area() & cur_insc %in% filtro_cur_insc(), .(Tipo_ens_med)][, .(n = .N), keyby = .(Tipo_ens_med)][, `:=`(prop = 100 *(n/sum(n)))][Tipo_ens_med == "Técnico", .(prop)],dados_reativo()[Candidato == "Matriculado"  & cur_mat %in% filtro_cur_insc() & area_mat %in% filtro_area() , .(Tipo_ens_med)][, .(n = .N), keyby = .(Tipo_ens_med)][,`:=`(prop = 100 * (n/sum(n)))][Tipo_ens_med == "Técnico", .(prop)])
    
    valor = 
      if(nrow(dados_p_tecnico)== 1 & sum(is.na(dados_p_tecnico)) == 0){
        dados_p_tecnico[,round(.SD,1),.SDcols = 1:2]
      } else if(nrow(dados_p_tecnico)== 1 & sum(is.na(dados_p_tecnico)) == 1){
        dados_p_tecnico[,round(.SD,1),.SDcols = 1][,round(.SD,1),.SDcols = 1][,propp := 0]
      } else 
        data.table(0,0)
    
    #if(nrow(dados_p_tecnico)==1){
    #  dados_p_tecnico[,round(.SD,1),.SDcols = 1:2]
    #} else data.table(0,0)
    
    bs4ValueBox(color = "danger",value = tags$p(style = "font-size: 25px;", "Ensino Médio Técnico"),
                subtitle = tags$p(style = "font-size: 20px", "Inscritos: ",
                                  valor[,1],"%",HTML("<br>"),"Matriculados: ",valor[,2],"%"
                ),icon = icon('pencil-ruler'))
  })
  
  output$p_cursinho <- renderbs4ValueBox({
    
    dados_p_cursinho = cbind( unique(dados_reativo(), by = c("Ano", "ID"))[area_insc %in% filtro_area() & cur_insc %in% filtro_cur_insc(), .(Cursinho)][, .(n = .N), keyby = .(Cursinho)][, `:=`(prop = 100 * (n/sum(n)))][Cursinho == "Sim", .(prop)],dados_reativo()[Candidato == "Matriculado"  & cur_mat %in% filtro_cur_insc() & area_mat %in% filtro_area() , .(Cursinho)][, .(n = .N), keyby = .(Cursinho)][, `:=`(prop = 100 * (n/sum(n)))][Cursinho == "Sim", .(prop)])
    
    valor = 
      if(nrow(dados_p_cursinho)== 1 & sum(is.na(dados_p_cursinho)) == 0){
        dados_p_cursinho[,round(.SD,1),.SDcols = 1:2]
      } else if(nrow(dados_p_cursinho)== 1 & sum(is.na(dados_p_cursinho)) == 1){
        dados_p_cursinho[,round(.SD,1),.SDcols = 1][,round(.SD,1),.SDcols = 1][,propp := 0]
      } else 
        data.table(0,0)
    #  if(nrow(dados_p_cursinho)==1){
    #    dados_p_cursinho[,round(.SD,1),.SDcols = 1:2]
    #  } else data.table(0,0)
    
    bs4ValueBox(color = "danger",value = tags$p(style = "font-size: 25px;", "Fez Cursinho"),
                subtitle = tags$p(style = "font-size: 20px", "Inscritos: ",
                                  valor[,1],"%",HTML("<br>"),"Matriculados: ",valor[,2],"%"
                ),icon = icon('chalkboard-teacher'))
  })
  
  output$p_inscritos <- renderbs4ValueBox({
    
    dados_p_inscritos = cbind(dados_reativo()[area_insc %in% filtro_area() & cur_insc %in% filtro_cur_insc(),.(n = .N)], unique(dados_reativo(), by = c("Ano", "ID"))[area_insc %in% filtro_area() & cur_insc %in% filtro_cur_insc(),.(n = .N)],dados_reativo()[Candidato == "Matriculado"  & cur_mat %in% filtro_cur_insc() & area_mat %in% filtro_area() ,.(n = .N)])
    
    valor = 
      if(nrow(dados_p_inscritos)==1){
        dados_p_inscritos
      } else data.table(0,0,0)
    
    bs4ValueBox(color = "danger",value = tags$p(style = "font-size: 25px;", "Candidatos"),
                subtitle = tags$p(style = "font-size: 20px", "Inscrições:  ",
                                  valor[,1],HTML("<br>"),"Inscritos:  ",valor[,2],HTML("<br>"),"Matriculados:  ",ifelse(is.na(valor[,3]),0,valor[,3])),icon = icon('chalkboard-teacher'))
  })
  
  output$p_indigena  <- renderbs4ValueBox({
    
    dados_p_indigena = cbind(unique( unique(dados_reativo(), by = c("Ano", "ID"))[area_insc %in% filtro_area() & cur_insc %in% filtro_cur_insc(), .(Cor)][, .(n = .N), keyby = .(Cor)][, `:=`(prop = 100 *(n/sum(n)))][Cor =="Indígena"][, `:=`(prop = sum(prop))][,.(prop)]),unique(dados_reativo()[Candidato == "Matriculado" & cur_mat %in% filtro_cur_insc() & area_mat %in% filtro_area() , .(Cor)][, .(n = .N),keyby = .(Cor)][, `:=`(prop = 100 * (n/sum(n)))][Cor =="Indígena"][, `:=`(prop = sum(prop))][, .(prop)]))
    
    valor = 
      if(nrow(dados_p_indigena)== 1 & sum(is.na(dados_p_indigena)) == 0){
        dados_p_indigena[,round(.SD,1),.SDcols = 1:2]
      } else if(nrow(dados_p_indigena)== 1 & sum(is.na(dados_p_indigena)) == 1){
        dados_p_indigena[,round(.SD,1),.SDcols = 1][,round(.SD,1),.SDcols = 1][,propp := 0]
      } else 
        data.table(0,0)
    
    bs4ValueBox(color = "danger",value = tags$p(style = "font-size: 25px;", "Indígenas"),
                subtitle = tags$p(style = "font-size: 20px", "Inscritos: ",
                                  valor[,1],"%",HTML("<br>"),"Matriculados: ",valor[,2],"%"
                ),icon = icon('user-friends')
    )
  })
  
  
  
  # PERFIL SOCIECONOMICO
  output$perfil_inscritos <- renderUI({
    
    if(input$cruzamento_perfil == "Processo"){
      hchart(
        dados_reativo()[cur_insc %in% filtro_cur_insc() & area_insc %in% filtro_area(), .(n = .N), keyby = c(input$variavel_perfil, input$cruzamento_perfil)][, `:=`(percent = round(100 * 
                                                                                                                                                                                       (n/sum(n)), 2)), by = c(input$variavel_perfil)]
        ,"bar",hcaes(!!sym(input$variavel_perfil),percent,group = !!sym(input$cruzamento_perfil) )) %>% 
        hc_yAxis(title = list(text = ""),labels = list(enabled = FALSE)) %>%
        hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '10px',fontWeight= 'bold')),type = 'category') %>% 
        hc_plotOptions(series = list(centerInCategory= TRUE,pointWidth = 30,stacking = "percent",borderWidth = 1,borderColor = "black", dataLabels = list(enabled = T,format = '{y} %',style = list(fontSize = '13px')))) %>% 
        hc_tooltip( 
          useHTML  =TRUE,
          headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
          pointFormat= "<span <span style='color:{series.color};font-size:15px'>{series.name} </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
          style = list(fontSize = '14px'),
          backgroundColor = "#ffffff",
          borderColor= '#000000',
          table = T
        ) %>% 
        hc_legend(align =  'center',verticalAlign =  'top') %>%
        hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções"))) %>% 
        hc_colors(c("#c12e34","#dab629","#367086","#2b821d","#32a487","#f6870f","#9026be","#d0541b","#1e1fd5","#075012","#f10707","#fbd200","#050065","#650000","#80ac6d","#8b5f8b")) %>% 
        hc_caption(text = HTML(testando[[input$variavel_perfil]]), useHTML = TRUE) %>% 
        hc_size(height = "80%")%>% 
        hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right"))
      
    } else if(input$variavel_perfil != "Processo" & input$cruzamento_perfil != "Processo" & input$cruzamento_perfil != "Nenhuma"){
      hchart(
        unique(dados_reativo(), by = c("Ano", "ID"))[cur_insc %in% filtro_cur_insc() & area_insc %in% filtro_area(), .(n = .N), keyby = c(input$variavel_perfil, input$cruzamento_perfil)][, `:=`(percent = round(100 * 
                                                                                                                                                                                                                    (n/sum(n)), 2)), by = c(input$variavel_perfil)]
        ,"bar",hcaes(!!sym(input$variavel_perfil),percent,group = !!sym(input$cruzamento_perfil) )) %>% 
        hc_yAxis(title = list(text = ""),labels = list(enabled = FALSE)) %>%
        hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '10px',fontWeight= 'bold')),type = 'category') %>% 
        hc_plotOptions(series = list(centerInCategory= TRUE,pointWidth = 30,stacking = "percent",borderWidth = 1,borderColor = "black", dataLabels = list(enabled = T,format = '{y} %',style = list(fontSize = '13px')))) %>% 
        hc_tooltip( 
          useHTML  =TRUE,
          headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
          pointFormat= "<span <span style='color:{series.color};font-size:15px'>{series.name} </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
          style = list(fontSize = '14px'),
          backgroundColor = "#ffffff",
          borderColor= '#000000',
          table = T
        ) %>% 
        hc_legend(align =  'center',verticalAlign =  'top') %>%
        hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções"))) %>% 
        hc_colors(c("#c12e34","#dab629","#367086","#2b821d","#32a487","#f6870f","#9026be","#d0541b","#1e1fd5","#075012","#f10707","#fbd200","#050065","#650000","#80ac6d","#8b5f8b")) %>% 
        hc_caption(text = HTML(testando[[input$variavel_perfil]]), useHTML = TRUE) %>% 
        hc_size(height = "80%")%>% 
        hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right"))
      
    } else if(input$cruzamento_perfil  == "Nenhuma" & input$variavel_perfil != "Processo"){
      hchart(name = "Porcentagem",
             #dados_reativo()[, .(n = .N), by = c(input$variavel_perfil)][, `:=`(percent = round(100 *(n/sum(n)), 1))]
             as.data.frame(dados_reativo())%>% group_by(Ano) %>% distinct(ID,.keep_all = TRUE) %>% filter(area_insc %in% filtro_area(),cur_insc %in% filtro_cur_insc()) %>% group_by(!!sym(input$variavel_perfil)) %>% summarise(n = n()) %>% mutate(percent = round(100*(n/sum(n)),2))
             ,"column",hcaes(x = !!sym(input$variavel_perfil), y = percent)) %>% 
        hc_yAxis(min=0,max=100,title = list(text = ""), labels = list(style = list(fontSize = '12px'),
                                                                      formatter = JS(
                                                                        "function(){
        perc = Math.round(this.value)
        return (perc + '%') 
        }"
                                                                      )
        )) %>%
        hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '15px',fontWeight= 'bold')),type = 'category' ) %>% 
        hc_plotOptions(series = list(borderWidth = 1,borderColor = "black", dataLabels = list(enabled = T,format = '{y}%',style = list(fontSize = '12px')))) %>% 
        hc_tooltip(
          useHTML  =TRUE,
          headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
          pointFormat= "<span <span style='font-size:15px'>Total </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
          style = list(fontSize = '14px'),
          backgroundColor = "#ffffff",
          borderColor= '#000000'
            
        ) %>%
        hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções")), filename =  'Gráfico_Comvest') %>% 
        hc_colors("#367086") %>% 
        hc_caption(text = HTML(testando[[input$variavel_perfil]]), useHTML = TRUE) %>% 
        hc_size(height = "80%") %>% 
        hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right"))
      
    } else if(input$variavel_perfil == "Processo" & input$cruzamento_perfil != "Nenhuma"){
      hchart(
        dados_reativo()[cur_insc %in% filtro_cur_insc() & area_insc %in% filtro_area(), .(n = .N), keyby = c(input$variavel_perfil, input$cruzamento_perfil)][, `:=`(percent = round(100 * 
                                                                                                                                                                                       (n/sum(n)), 2)), by = c(input$variavel_perfil)]
        ,"bar",hcaes(!!sym(input$variavel_perfil),percent,group = !!sym(input$cruzamento_perfil) )) %>% 
        hc_yAxis(title = list(text = ""),labels = list(enabled = FALSE)) %>%
        hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '10px',fontWeight= 'bold')),type = 'category') %>% 
        hc_plotOptions(series = list(centerInCategory= TRUE,pointWidth = 30,stacking = "percent",borderWidth = 1,borderColor = "black", dataLabels = list(enabled = T,format = '{y} %',style = list(fontSize = '13px')))) %>% 
        hc_tooltip( 
          useHTML  =TRUE,
          headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
          pointFormat= "<span <span style='color:{series.color};font-size:15px'>{series.name} </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
          style = list(fontSize = '14px'),
          backgroundColor = "#ffffff",
          borderColor= '#000000',
          table = T
        ) %>% 
        hc_legend(align =  'center',verticalAlign =  'top') %>%
        hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções"))) %>% 
        hc_colors(c("#c12e34","#dab629","#367086","#2b821d","#32a487","#f6870f","#9026be","#d0541b","#1e1fd5","#075012","#f10707","#fbd200","#050065","#650000","#80ac6d","#8b5f8b")) %>% 
        hc_caption(text = HTML(testando[[input$variavel_perfil]]), useHTML = TRUE) %>% 
        hc_size(height = "80%")%>% 
        hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right"))
      
    } else{
      hchart(name = "Porcentagem",
             #dados_reativo()[, .(n = .N), by = c(input$variavel_perfil)][, `:=`(percent = round(100 *(n/sum(n)), 1))]
             as.data.frame(dados_reativo()) %>% filter(area_insc %in% filtro_area(),cur_insc %in% filtro_cur_insc()) %>% group_by(!!sym(input$variavel_perfil)) %>% summarise(n = n()) %>% mutate(percent = round(100*(n/sum(n)),2))
             ,"column",hcaes(x = !!sym(input$variavel_perfil), y = percent)) %>% 
        hc_yAxis(min=0,max=100,title = list(text = ""), labels = list(style = list(fontSize = '12px'),
                                                                      formatter = JS(
                                                                        "function(){
        perc = Math.round(this.value)
        return (perc + '%') 
        }"
                                                                      )
        )) %>%
        hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '15px',fontWeight= 'bold')),type = 'category' ) %>% 
        hc_plotOptions(series = list(borderWidth = 1,borderColor = "black", dataLabels = list(enabled = T,format = '{y}%',style = list(fontSize = '12px')))) %>% 
        hc_tooltip(
          useHTML  =TRUE,
          headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
          pointFormat= "<span <span style='font-size:15px'>Total </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
          style = list(fontSize = '14px'),
          backgroundColor = "#ffffff",
          borderColor= '#000000'
            
        ) %>%
        hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções")), filename =  'Gráfico_Comvest') %>% 
        hc_colors("#367086") %>% 
        hc_caption(text = HTML(testando[[input$variavel_perfil]]), useHTML = TRUE) %>% 
        hc_size(height = "80%") %>% 
        hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right"))
      
    }
    
  })
  
  output$perfil_matriculados <- renderUI({
    
    tryCatch(
      if(input$cruzamento_perfil != "Nenhuma" ){
        
        hchart(
          
          dados_reativo()[Candidato == "Matriculado" & cur_mat %in% filtro_cur_insc() & area_mat %in% filtro_area(), .(n = .N), keyby = c(input$variavel_perfil, input$cruzamento_perfil)][, `:=`(percent = round(100 * (n/sum(n)), 2)), by = c(input$variavel_perfil)]
          ,"bar",hcaes(!!sym(input$variavel_perfil),percent,group = !!sym(input$cruzamento_perfil))) %>%
          hc_yAxis(title = list(text = ""),labels = list(enabled = FALSE)) %>%
          hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '15px',fontWeight= 'bold')) ) %>% 
          hc_plotOptions(series = list(centerInCategory= TRUE,pointWidth = 30,stacking = "percent",borderWidth = 1,borderColor = "black", dataLabels = list(enabled = T,format = '{y} %',style = list(fontSize = '13px')))) %>% 
          hc_tooltip( 
            useHTML  =TRUE,
            headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
            pointFormat= "<span <span style='color:{series.color};font-size:15px'>{series.name} </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
            style = list(fontSize = '14px'),
            backgroundColor = "#ffffff",
            borderColor= '#000000',
            table = T
          ) %>% 
          hc_legend(align =  'center',verticalAlign =  'top') %>%
          hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções"))) %>% 
          hc_colors(c("#c12e34","#dab629","#367086","#2b821d","#32a487","#f6870f","#9026be","#d0541b","#1e1fd5","#075012","#f10707","#fbd200","#050065","#650000","#80ac6d","#8b5f8b")) %>% 
          hc_caption(text = HTML(testando[[input$variavel_perfil]]), useHTML = TRUE) %>% 
          hc_size(height = "80%")%>% 
          hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right"))
        
        
        
      } else {
        
        
        #dados_perfil_inscrito = dados %>% filter(candidato =="inscrito")  %>% group_by_at(vars(variavel_perfil))  %>% summarise(n=n()) %>% mutate(percent= round(100*(n/sum(n)),1))
        
        hchart(name = "Porcentagem",
               #dados_reativo()[Candidato =="Matriculado", .(n = .N), by = c(input$variavel_perfil)][, `:=`(percent = round(100 * (n/sum(n)), 1))]
               as.data.frame(dados_reativo()) %>% filter(Candidato =="Matriculado",area_mat %in% filtro_area(),cur_mat %in% filtro_cur_insc()) %>% group_by(!!sym(input$variavel_perfil)) %>% summarise(n = n()) %>% mutate(percent = round(100*(n/sum(n)),2))
               ,"column",hcaes(x = !!sym(input$variavel_perfil), y = percent)) %>%
          hc_yAxis(min=0,max=100,title = list(text = ""), labels = list(style = list(fontSize = '12px'),
                                                                        formatter = JS(
                                                                          "function(){
        perc = Math.round(this.value)
        return (perc + '%') 
        }"
                                                                        )
          )) %>%
          hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '15px',fontWeight= 'bold'))) %>% 
          hc_plotOptions(series = list(borderWidth = 1,borderColor = "black", dataLabels = list(enabled = T,format = '{y}%',style = list(fontSize = '12px')))) %>% 
          hc_tooltip(
            useHTML  =TRUE,
            headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
            pointFormat= "<span <span style='font-size:15px'>Total </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
            style = list(fontSize = '14px'),
            backgroundColor = "#ffffff",
            borderColor= '#000000'
          ) %>%
          hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções"))) %>% 
          hc_colors("#075012") %>% 
          hc_caption(text = HTML(testando[[input$variavel_perfil]]), useHTML = TRUE) %>% 
          hc_size(height = "80%")%>% 
          hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right"))
      },error = function(e) "Esse Curso não consta no catálogo de cursos da UNICAMP!")
    
  })
  
  
  
  
  # FILTROS - SERIE
  filtro_area_s       <- reactive({if(is.null(input$area_s))      levels(dados[,area_insc]) else input$area_s})
  filtro_cur_insc_s   <- reactive({if(is.null(input$cur_insc_s))  levels(dados[,cur_insc]) else input$cur_insc_s})
  filtro_processo_s   <- reactive({if(is.null(input$processo_s))  levels(dados[,Processo]) else input$processo_s})
  filtro_isento_s     <- reactive({if(is.null(input$isento_s))    levels(dados[,Isento]) else input$isento_s})
  filtro_sexo_s       <- reactive({if(is.null(input$sexo_s))      levels(dados[,Sexo]) else input$sexo_s})
  filtro_cor_s        <- reactive({if(is.null(input$cor_s))       levels(dados[,Cor]) else input$cor_s})
  
  
  # DADOS FILTRADOS - SERIE
  dados_reativo_s <- reactive({
    
    dados[
      #area_insc       %in% filtro_area_s()       &
      #cur_insc        %in% filtro_cur_insc_s()   &
      Processo        %in% filtro_processo_s()    &
        Isento          %in% filtro_isento_s()      &
        Sexo            %in% filtro_sexo_s()        &
        Cor             %in% filtro_cor_s(),]
    
  })
  
  
  
  # FILTROS INTERLIGADOS - SERIE
  observeEvent(input$area_s,{
    updatePickerInput(session,inputId = "cur_insc_s",choices = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$area_insc %in% input$area_s)])]%>% sort(),selected = input$cur_insc_s)
    updatePickerInput(session,inputId = "processo_s",choices = levels(dados$Processo)[unique(dados$Processo[which(dados$area_insc %in% input$area_s)])]%>% sort(),selected = input$processo_s)
    updatePickerInput(session,inputId = "isento_s",choices =  levels(dados$Isento)[unique(dados$Isento[which(dados$area_insc %in% input$area_s)])]%>% sort(),selected = input$isento_s)
    updatePickerInput(session,inputId = "sexo_s",choices = levels(dados$Sexo)[unique(dados$Sexo[which(dados$area_insc %in% input$area_s)])]%>% sort(),selected = input$sexo_s)
    updatePickerInput(session,inputId = "cor_s",choices =  levels(dados$Cor)[unique(dados$Cor[which(dados$area_insc %in% input$area_s)])]%>% sort(),selected = input$cor_s) 
  })
  
  observeEvent(input$cur_insc_s,{
    updatePickerInput(session,inputId = "area_s",choices = levels(dados$area_insc)[unique(dados$area_insc[which(dados$cur_insc %in% input$cur_insc_s)])]%>% sort(),selected = input$area_s)
    updatePickerInput(session,inputId = "processo_s",choices = levels(dados$Processo)[unique(dados$Processo[which(dados$cur_insc %in% input$cur_insc_s)])]%>% sort(),selected = input$processo_s)
    updatePickerInput(session,inputId = "isento_s",choices = levels(dados$Isento)[unique(dados$Isento[which(dados$cur_insc %in% input$cur_insc_s)])]%>% sort(),selected = input$isento_s)
    updatePickerInput(session,inputId = "sexo_s",choices =levels(dados$Sexo)[unique(dados$Sexo[which(dados$cur_insc %in% input$cur_insc_s)])]%>% sort(),selected = input$sexo_s)
    updatePickerInput(session,inputId = "cor_s",choices = levels(dados$Cor)[unique(dados$Cor[which(dados$cur_insc %in% input$cur_insc_s)])]%>% sort(),selected = input$cor_s) 
    
  })
  
  observeEvent(input$processo_s,{
    updatePickerInput(session,inputId = "area_s",choices = levels(dados$area_insc)[unique(dados$area_insc[which(dados$Processo %in% input$processo_s)])]%>% sort(),selected = input$area_s)
    updatePickerInput(session,inputId = "cur_insc_s",choices = list(
      "Artes" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Processo %in% input$processo_s & dados$area_insc == "Artes")])]%>% sort(),
      "Ciências Biológicas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Processo %in% input$processo_s & dados$area_insc %in% c("Ciências Biológicas","Medicina"))])]%>% sort(),
      "Ciências Exatas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Processo %in% input$processo_s & dados$area_insc == "Ciências Exatas")])]%>% sort(),
      "Ciências Humanas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Processo %in% input$processo_s & dados$area_insc == "Ciências Humanas")])]%>% sort(),
      "Engenharias" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Processo %in% input$processo_s & dados$area_insc == "Engenharias")])]%>% sort(),
      "Tecnológias" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Processo %in% input$processo_s & dados$area_insc == "Tecnológias")])]%>% sort(),
      "Profis" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Processo %in% input$processo_s & dados$area_insc == "Profis")])]%>% sort()
    ),selected = input$cur_insc_s)
    #updatePickerInput(session,inputId = "cur_insc_s",choices = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Processo %in% input$processo_s)])]%>% sort(),selected = input$cur_insc_s)
    updatePickerInput(session,inputId = "isento_s",choices = levels(dados$Isento)[unique(dados$Isento[which(dados$Processo %in% input$processo_s)])]%>% sort(),selected = input$isento_s)
    updatePickerInput(session,inputId = "sexo_s",choices =levels(dados$Sexo)[unique(dados$Sexo[which(dados$Processo %in% input$processo_s)])]%>% sort(),selected = input$sexo_s)
    updatePickerInput(session,inputId = "cor_s",choices = levels(dados$Cor)[unique(dados$Cor[which(dados$Processo %in% input$processo_s)])]%>% sort(),selected = input$cor_s) 
  })
  
  observeEvent(input$isento_s,{
    updatePickerInput(session,inputId = "area_s",choices = levels(dados$area_insc)[unique(dados$area_insc[which(dados$Isento %in% input$isento_s)])]%>% sort(),selected = input$area_s)
    updatePickerInput(session,inputId = "cur_insc_s",choices = list(
      "Artes" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Isento %in% input$isento_s & dados$area_insc == "Artes")])]%>% sort(),
      "Ciências Biológicas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Isento %in% input$isento_s & dados$area_insc %in% c("Ciências Biológicas","Medicina"))])]%>% sort(),
      "Ciências Exatas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Isento %in% input$isento_s & dados$area_insc == "Ciências Exatas")])]%>% sort(),
      "Ciências Humanas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Isento %in% input$isento_s & dados$area_insc == "Ciências Humanas")])]%>% sort(),
      "Engenharias" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Isento %in% input$isento_s & dados$area_insc == "Engenharias")])]%>% sort(),
      "Tecnológias" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Isento %in% input$isento_s & dados$area_insc == "Tecnológias")])]%>% sort(),
      "Profis" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Isento %in% input$isento_s & dados$area_insc == "Profis")])]%>% sort()
      
    ),selected = input$cur_insc_s)
    #updatePickerInput(session,inputId = "cur_insc_s",choices = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Isento %in% input$isento_s)])]%>% sort(),selected = input$cur_insc_s)
    updatePickerInput(session,inputId = "processo_s",choices = levels(dados$Processo)[unique(dados$Processo[which(dados$Isento %in% input$isento_s)])]%>% sort(),selected = input$processo_s)
    updatePickerInput(session,inputId = "sexo_s",choices = levels(dados$Sexo)[unique(dados$Sexo[which(dados$Isento %in% input$isento_s)])]%>% sort(),selected = input$sexo_s)
    updatePickerInput(session,inputId = "cor_s",choices =  levels(dados$Cor)[unique(dados$Cor[which(dados$Isento %in% input$isento_s)])]%>% sort(),selected = input$cor_s) 
    
  })
  
  observeEvent(input$sexo_s,{
    updatePickerInput(session,inputId = "area_s",choices = levels(dados$area_insc)[unique(dados$area_insc[which(dados$Sexo %in% input$sexo_s)])]%>% sort(),selected = input$area_s)
    updatePickerInput(session,inputId = "cur_insc_s",choices = list(
      "Artes" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Sexo %in% input$sexo_s & dados$area_insc == "Artes")])]%>% sort(),
      "Ciências Biológicas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Sexo %in% input$sexo_s & dados$area_insc %in% c("Ciências Biológicas","Medicina"))])]%>% sort(),
      "Ciências Exatas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Sexo %in% input$sexo_s & dados$area_insc == "Ciências Exatas")])]%>% sort(),
      "Ciências Humanas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Sexo %in% input$sexo_s & dados$area_insc == "Ciências Humanas")])]%>% sort(),
      "Engenharias" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Sexo %in% input$sexo_s & dados$area_insc == "Engenharias")])]%>% sort(),
      "Tecnológias" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Sexo %in% input$sexo_s& dados$area_insc == "Tecnológias")])]%>% sort(),
      "Profis" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Sexo %in% input$sexo_s & dados$area_insc == "Profis")])]%>% sort()
    ),selected = input$cur_insc_s)
    #updatePickerInput(session,inputId = "cur_insc_s",choices = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Sexo %in% input$sexo_s)])]%>% sort(),selected = input$cur_insc_s)
    updatePickerInput(session,inputId = "processo_s",choices =levels(dados$Processo)[unique(dados$Processo[which(dados$Sexo %in% input$sexo_s)])]%>% sort(),selected = input$processo_s)
    updatePickerInput(session,inputId = "isento_s",choices = levels(dados$Isento)[unique(dados$Isento[which(dados$Sexo %in% input$sexo_s)])]%>% sort(),selected = input$isento_s)
    updatePickerInput(session,inputId = "cor_s",choices =  levels(dados$Cor)[unique(dados$Cor[which(dados$Sexo %in% input$sexo_s)])]%>% sort(),selected = input$cor_s) 
  })
  
  observeEvent(input$cor_s,{
    updatePickerInput(session,inputId = "area_s",choices = levels(dados$area_insc)[unique(dados$area_insc[which(dados$Cor %in% input$co_s)])]%>% sort(),selected = input$area_s)
    updatePickerInput(session,inputId = "cur_insc_s",choices = list(
      "Artes" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Cor %in% input$co_s & dados$area_insc == "Artes")])]%>% sort(),
      "Ciências Biológicas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Cor %in% input$co_s & dados$area_insc %in% c("Ciências Biológicas","Medicina"))])]%>% sort(),
      "Ciências Exatas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Cor %in% input$co_s & dados$area_insc == "Ciências Exatas")])]%>% sort(),
      "Ciências Humanas" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Cor %in% input$co_s & dados$area_insc == "Ciências Humanas")])]%>% sort(),
      "Engenharias" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Cor %in% input$co_s & dados$area_insc == "Engenharias")])]%>% sort(),
      "Tecnológias" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Cor %in% input$co_s & dados$area_insc == "Tecnológias")])]%>% sort(),
      "Profis" = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Cor %in% input$co_s & dados$area_insc == "Profis")])]%>% sort()
    ),selected = input$cur_insc_s)
    #updatePickerInput(session,inputId = "cur_insc_s",choices = levels(dados$cur_insc)[unique(dados$cur_insc[which(dados$Cor %in% input$cor_s)])]%>% sort(),selected = input$cur_insc_s)
    updatePickerInput(session,inputId = "processo_s",choices = levels(dados$Processo)[unique(dados$Processo[which(dados$Cor %in% input$cor_s)])]%>% sort(),selected = input$processo_s)
    updatePickerInput(session,inputId = "isento_s",choices = levels(dados$Isento)[unique(dados$Isento[which(dados$Cor %in% input$cor_s)])]%>% sort(),selected = input$isento_s)
    updatePickerInput(session,inputId = "sexo_s",choices = levels(dados$Sexo)[unique(dados$Sexo[which(dados$Cor %in% input$cor_s)])]%>% sort(),selected = input$sexo_s)
  }) 
  
  
  
  # RESET INPUT - PERFIL
  observe({
    req(input$reset_input_s)
    updatePickerInput(session,inputId = "area_s",choices = levels(dados$area_insc),selected = NULL)
    updatePickerInput(session,inputId = "cur_insc_s",choices =list(
      "Artes" = levels(dados$cur_insc[which(dados$area_insc == "Artes")])[unique(dados$cur_insc[which(dados$area_insc == "Artes")])] %>% sort(),
      "Ciências Biológicas" = levels(dados$cur_insc[which(dados$area_insc %in% c("Ciências Biológicas","Medicina"))])[unique(dados$cur_insc[which(dados$area_insc %in% c("Ciências Biológicas","Medicina"))])] %>% sort(),
      "Ciências Exatas" = levels(dados$cur_insc[which(dados$area_insc == "Ciências Exatas")])[unique(dados$cur_insc[which(dados$area_insc == "Ciências Exatas")])] %>% sort(),
      "Ciências Humanas" = levels(dados$cur_insc[which(dados$area_insc == "Ciências Humanas")])[unique(dados$cur_insc[which(dados$area_insc == "Ciências Humanas")])] %>% sort(),
      "Engenharias" = levels(dados$cur_insc[which(dados$area_insc == "Engenharias")])[unique(dados$cur_insc[which(dados$area_insc == "Engenharias")])] %>% sort(),
      "Tecnológias" = levels(dados$cur_insc[which(dados$area_insc == "Tecnológias")])[unique(dados$cur_insc[which(dados$area_insc == "Tecnológias")])] %>% sort(),
      "Profis" = levels(dados$cur_insc[which(dados$area_insc == "Profis")])[unique(dados$cur_insc[which(dados$area_insc == "Profis")])] %>% sort()
    ),selected =NULL)
    updatePickerInput(session,inputId = "processo_s",choices = levels(dados$Processo),selected = NULL)
    updatePickerInput(session,inputId = "isento_s",choices = levels(dados$Isento),selected =NULL)
    updatePickerInput(session,inputId = "sexo_s",choices = levels(dados$Sexo),selected = NULL)
    updatePickerInput(session,inputId = "cor_s",choices = levels(dados$Cor),selected =NULL)
  })
  
  
  
  # SERIE HISTORICA
  output$serie_inscritos <-  renderUI({
    
    
    dreact = unique(dados_reativo_s(), by = c("Ano", "ID"))[area_insc %in% filtro_area_s() & cur_insc %in% filtro_cur_insc_s(), .(n = .N), by = c("Ano",input$variavel_serie)][, `:=`(perc = round(100*(n/sum(n)), 1)),keyby = c("Ano")][,c("Ano",input$variavel_serie):= lapply(.SD, as.character),.SDcols = c("Ano",input$variavel_serie)][]
    dreact_sem_filtro = dados_reativo_s()[area_insc %in% filtro_area_s() & cur_insc %in% filtro_cur_insc_s(), .(n = .N), by = c("Ano",input$variavel_serie)][, `:=`(perc = round(100*(n/sum(n)), 1)),keyby = c("Ano")][,c("Ano",input$variavel_serie):= lapply(.SD, as.character),.SDcols = c("Ano",input$variavel_serie)][]
    
    setnames(dreact,old = input$variavel_serie, new = "Grupo")
    setnames(dreact_sem_filtro,old = input$variavel_serie, new = "Grupo")
    
    
    if((input$tipo_graf_serie == "line" & input$variavel_serie != "Processo")){
      
      hchart(dreact[CJ(Ano = dreact[["Ano"]]  , Grupo = dreact[["Grupo"]] ,unique = TRUE), on = c("Ano","Grupo")]
             [, n := nafill(n,type = "const",fill = 0),by = c("Ano")]
             [, perc := nafill(perc,type = "const",fill = 0),by = c("Ano")][],
             type ="line",
             hcaes(x = Ano,y = perc,group = Grupo)) %>%
        hc_tooltip(table = T,useHTML  =TRUE,
                   headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
                   pointFormat= "<span <span style='color:{series.color};font-size:15px'>{series.name} </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
                   style = list(fontSize = '14px'),
                   backgroundColor = "#ffffff",
                   borderColor= '#000000'
        )  %>% 
        hc_chart(zoomType = "x", panning= TRUE,panKey = 'shift') %>% 
        hc_legend(align =  'center',verticalAlign =  'top',x= 0,y= -15) %>% 
        hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '12px',fontWeight= 'bold')))%>%
        hc_yAxis(max=100,title = list(text = ""), labels = list(style = list(fontSize = '12px'),
                                                                formatter = JS(
                                                                  "function(){
        perc = Math.round(this.value)
        return (perc + '%') 
        }"
                                                                )
        )) %>%
        hc_colors(c("#c12e34","#dab629","#367086","#2b821d","#32a487","#f6870f","#9026be","#d0541b","#1e1fd5","#075012","#f10707","#fbd200","#050065","#650000","#80ac6d","#8b5f8b")) %>% 
        hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções"))) %>% 
        hc_caption(text = HTML(testando[[input$variavel_serie]]), useHTML = TRUE) %>% 
        hc_size(height = "80%")%>% 
        hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right")) 
      
    } else if(input$tipo_graf_serie == "line" & input$variavel_serie == "Processo"){
      
      hchart(dreact_sem_filtro[CJ(Ano = dreact_sem_filtro[["Ano"]]  , Grupo = dreact_sem_filtro[["Grupo"]] ,unique = TRUE), on = c("Ano","Grupo")]
             [, n := nafill(n,type = "const",fill = 0),by = c("Ano")]
             [, perc := nafill(perc,type = "const",fill = 0),by = c("Ano")][],
             type ="line",
             hcaes(x = Ano,y = perc,group = Grupo)) %>%
        hc_tooltip(table = T,useHTML  =TRUE,
                   headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
                   pointFormat= "<span <span style='color:{series.color};font-size:15px'>{series.name} </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
                   style = list(fontSize = '14px'),
                   backgroundColor = "#ffffff",
                   borderColor= '#000000'
        )  %>% 
        hc_chart(zoomType = "x", panning= TRUE,panKey = 'shift') %>% 
        hc_legend(align =  'center',verticalAlign =  'top',x= 0,y= -15) %>% 
        hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '12px',fontWeight= 'bold')))%>%
        hc_yAxis(max=100,title = list(text = ""), labels = list(style = list(fontSize = '12px'),
                                                                formatter = JS(
                                                                  "function(){
        perc = Math.round(this.value)
        return (perc + '%') 
        }"
                                                                )
        )) %>%
        hc_colors(c("#c12e34","#dab629","#367086","#2b821d","#32a487","#f6870f","#9026be","#d0541b","#1e1fd5","#075012","#f10707","#fbd200","#050065","#650000","#80ac6d","#8b5f8b")) %>% 
        hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções"))) %>% 
        hc_caption(text = HTML(testando[[input$variavel_serie]]), useHTML = TRUE) %>% 
        hc_size(height = "80%")%>% 
        hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right")) 
      
    } else if(input$tipo_graf_serie == "bar" & input$variavel_serie != "Processo"){
      
      hchart(dreact[CJ(Ano = dreact[["Ano"]]  , Grupo = dreact[["Grupo"]] ,unique = TRUE), on = c("Ano","Grupo")]
             [, n := nafill(n,type = "const",fill = 0),by = c("Ano")]
             [, perc := nafill(perc,type = "const",fill = 0),by = c("Ano")][],
             type ="bar",
             hcaes(x = Ano,y = perc,group = Grupo)) %>%
        hc_tooltip(table = T,useHTML  =TRUE,
                   headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
                   pointFormat= "<span <span style='color:{series.color};font-size:15px'>{series.name} </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
                   style = list(fontSize = '14px'),
                   backgroundColor = "#ffffff",
                   borderColor= '#000000'
        )  %>% 
        hc_chart(zoomType = "xy") %>% 
        hc_legend(align =  'center',verticalAlign =  'top',x= 0,y= -15) %>% 
        hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '12px',fontWeight= 'bold')))%>%
        hc_yAxis(title = list(text = ""),labels = list(enabled = FALSE)) %>%
        hc_plotOptions(series = list( maxPointWidth = 10,stacking = "percent",centerInCategory= TRUE,pointWidth = 13,borderWidth = 1,borderColor = "black"),style = list(fontSize = '12px')) %>%
        hc_colors(c("#c12e34","#dab629","#367086","#2b821d","#32a487","#f6870f","#9026be","#d0541b","#1e1fd5","#075012","#f10707","#fbd200","#050065","#650000","#80ac6d","#8b5f8b")) %>% 
        hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções"))) %>% 
        hc_caption(text = HTML(testando[[input$variavel_serie]]), useHTML = TRUE) %>% 
        hc_size(height = "80%")%>% 
        hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right"))
      
    }  else if(input$tipo_graf_serie == "bar" & input$variavel_serie == "Processo"){
      
      hchart(dreact_sem_filtro[CJ(Ano = dreact_sem_filtro[["Ano"]]  , Grupo = dreact_sem_filtro[["Grupo"]] ,unique = TRUE), on = c("Ano","Grupo")]
             [, n := nafill(n,type = "const",fill = 0),by = c("Ano")]
             [, perc := nafill(perc,type = "const",fill = 0),by = c("Ano")][],
             type ="bar",
             hcaes(x = Ano,y = perc,group = Grupo)) %>%
        hc_tooltip(table = T,useHTML  =TRUE,
                   headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
                   pointFormat= "<span <span style='color:{series.color};font-size:15px'>{series.name} </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
                   style = list(fontSize = '14px'),
                   backgroundColor = "#ffffff",
                   borderColor= '#000000'
        )  %>% 
        hc_chart(zoomType = "xy") %>% 
        hc_legend(align =  'center',verticalAlign =  'top',x= 0,y= -15) %>% 
        hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '12px',fontWeight= 'bold')))%>%
        hc_yAxis(title = list(text = ""),labels = list(enabled = FALSE)) %>%
        hc_plotOptions(series = list( maxPointWidth = 10,stacking = "percent",centerInCategory= TRUE,pointWidth = 13,borderWidth = 1,borderColor = "black"),style = list(fontSize = '12px')) %>%
        hc_colors(c("#c12e34","#dab629","#367086","#2b821d","#32a487","#f6870f","#9026be","#d0541b","#1e1fd5","#075012","#f10707","#fbd200","#050065","#650000","#80ac6d","#8b5f8b")) %>% 
        hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções"))) %>% 
        hc_caption(text = HTML(testando[[input$variavel_serie]]), useHTML = TRUE) %>% 
        hc_size(height = "80%")%>% 
        hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right")) 
    }
    
    
    
  })
  
  output$serie_matriculados <-  renderUI({
    
    dreact = dados_reativo_s()[Candidato =="Matriculado" & area_insc %in% filtro_area_s()& cur_mat %in% filtro_cur_insc_s(), .(n = .N), by = c("Ano",input$variavel_serie)][, `:=`(perc = round(100*(n/sum(n)), 1)),keyby = c("Ano")][,c("Ano",input$variavel_serie):= lapply(.SD, as.character),.SDcols = c("Ano",input$variavel_serie)][]
    
    setnames(dreact,old = input$variavel_serie, new = "Grupo")
    
    
    if((input$tipo_graf_serie == "line")){
      
      hchart(
        dreact[CJ(Ano = dreact[["Ano"]]  , Grupo = dreact[["Grupo"]] ,unique = TRUE), on = c("Ano","Grupo")]
        [, n := nafill(n,type = "const",fill = 0),by = c("Ano")]
        [, perc := nafill(perc,type = "const",fill = 0),by = c("Ano")][]  ,
        type ="line",
        hcaes(x = Ano,y = perc,group = Grupo)) %>%
        hc_tooltip(table = T,useHTML  =TRUE,
                   headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
                   pointFormat= "<span <span style='color:{series.color};font-size:15px'>{series.name} </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
                   style = list(fontSize = '14px'),
                   backgroundColor = "#ffffff",
                   borderColor= '#000000'
        )  %>% 
        hc_chart(zoomType = "xy") %>% 
        hc_legend(align =  'center',verticalAlign =  'top',x= 0,y= -15) %>% 
        hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '12px',fontWeight= 'bold')))%>%
        hc_yAxis(max=100,title = list(text = ""), labels = list(style = list(fontSize = '12px'),
                                                                formatter = JS(
                                                                  "function(){
        perc = Math.round(this.value)
        return (perc + '%') 
        }"
                                                                )
        )) %>%
        hc_colors(c("#c12e34","#dab629","#367086","#2b821d","#32a487","#f6870f","#9026be","#d0541b","#1e1fd5","#075012","#f10707","#fbd200","#050065","#650000","#80ac6d","#8b5f8b")) %>% 
        hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções"))) %>% 
        hc_caption(text = HTML(testando[[input$variavel_serie]]), useHTML = TRUE)  %>% 
        hc_size(height = "80%")%>% 
        hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right"))
      
    }
    else if(input$tipo_graf_serie == "bar"){
      
      hchart(
        dreact[CJ(Ano = dreact[["Ano"]]  , Grupo = dreact[["Grupo"]] ,unique = TRUE), on = c("Ano","Grupo")]
        [, n := nafill(n,type = "const",fill = 0),by = c("Ano")]
        [, perc := nafill(perc,type = "const",fill = 0),by = c("Ano")][],
        type ="bar",
        hcaes(x = Ano,y = perc,group = Grupo)) %>%
        hc_tooltip(table = T,useHTML  =TRUE,
                   headerFormat= "<span style='font-size:20px;'><b>{point.key}</b></span><br>",
                   pointFormat= "<span <span style='color:{series.color};font-size:15px'>{series.name} </span> : <b>{point.n} ({point.y:.1f}%)</b> </span><br>",
                   style = list(fontSize = '14px'),
                   backgroundColor = "#ffffff",
                   borderColor= '#000000'
        )  %>% 
        hc_chart(zoomType = "xy") %>% 
        hc_legend(align =  'center',verticalAlign =  'top',x= 0,y= -15) %>% 
        hc_xAxis(title = list(text = ""), labels = list(style = list(fontSize = '12px',fontWeight= 'bold')))%>%
        hc_yAxis(title = list(text = ""),labels = list(enabled = FALSE)) %>%
        hc_plotOptions(series = list( maxPointWidth = 10,stacking = "percent",centerInCategory= TRUE,pointWidth = 13,borderWidth = 1,borderColor = "black"),style = list(fontSize = '12px')) %>%
        hc_colors(c("#c12e34","#dab629","#367086","#2b821d","#32a487","#f6870f","#9026be","#d0541b","#1e1fd5","#075012","#f10707","#fbd200","#050065","#650000","#80ac6d","#8b5f8b")) %>% 
        hc_exporting(enabled = TRUE,buttons = list(contextButton = list(text = "Opções"))) %>% 
        hc_caption(text = HTML(testando[[input$variavel_serie]]), useHTML = TRUE)  %>% 
        hc_size(height = "80%")%>% 
        hc_credits(enabled = TRUE,text = "© COMVEST", href = "https://www.comvest.unicamp.br/",position = list(align = "right"))}
    
    
    
  })
  
  
  # TEXTO DICIONÁRIO
  output$texto_dic <- renderUI({
    HTML(testando[[input$variavel_dic]])
  })
  
  
  # MAPA
  map_data <- reactive({
    # Criando os dados para o mapa:
    unique(setnames(setcolorder(setDT(lat_lon)[dados[Ano %in% input$ano_mapa & Processo %in% 
                                                       c("Vestibular Unicamp", "Vestibular Indígena"), .(Processo, 
                                                                                                         cid_inscricao)][, `:=`(c("cid_inscricao"), {
                                                                                                           cid_inscricao <- iconv(toupper(cid_inscricao), to = "ASCII//TRANSLIT")
                                                                                                           cid_inscricao <- fifelse(cid_inscricao %in% c("CEPRE", "ESPECIAIS"), 
                                                                                                                                    "CAMPINAS-SP", cid_inscricao)
                                                                                                           .(cid_inscricao)
                                                                                                         })], on = .(Cidade = cid_inscricao), allow.cartesian = TRUE], 
                                c(4L, 1L, 2L, 3L)), "Cidade", "cid_inscricao")[, `:=`(n = .N), 
                                                                               by = .(cid_inscricao, Processo)])
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-53, -15, zoom = 4.5) %>% 
      addPolygons(data = uf,
                  color = 'gray',
                  fillOpacity = 0.1,
                  weight  = 1.5) %>%
      addPolygons(data = sp,
                  color = 'green',
                  fillOpacity = 0.1,
                  weight  = 1.5) %>%
      addPolygons(data = cps,
                  color = 'blue',
                  fillOpacity = 0.1,
                  weight  = 1.5) %>%
      addAwesomeMarkers(data = map_data(),
                        lat = ~LATITUDE,
                        lng = ~LONGITUDE,
                        popup = ~paste0('<b>',cid_inscricao,'</b>', ':<br>', n, ' candidatos.'),
                        clusterOptions = markerClusterOptions(),
                        icon = ~IconSet[Processo]) %>% 
      addLegendAwesomeIcon(iconSet = IconSet,
                           title = 'Tipo de Ingresso',
                           position = 'bottomright',
                           group = 'Awesome Icons')
  })
  
  output$image <-renderImage({
    list(src = "comvest_logo.png",
         width = 200,
         height = 50,deleteFile=TRUE)
  })
  
  
  # DIFERENTES FORMAS DE INGRESSO 
  output$chart <- renderHighchart({
    
    load(paste0('comp_data_',input$ano_form_ingr,'.Rdata'))
    
    comp_data %>%  
      hchart(
        "column",
        hcaes(x = Processo, y = N), color = '#0066FF') %>%
      hc_title(text = 'Inscritos em mais de uma forma de Ingresso') %>%
      hc_subtitle(text = 'Considerando apenas as interações 2 x 2',
                  align = 'left') %>% 
      hc_plotOptions(
        column = list(
          pointWidth = 100,
          dataLabels = list(
            enabled = TRUE,
            color = '#404040',
            format = '{point.y}',
            shape = NULL,
            style = list(textOutline = NULL,
                         fontSize = '12px')))) %>% 
      hc_tooltip(
        useHTML = TRUE,
        positioner = JS("function (labelWidth, labelHeight, point) { return { x: this.chart.plotLeft , y: this.chart.plotTop}; }"),
        headerFormat = "<span style='font-size: 16px; color: #606060'> Somente {point.key} : {point.point.N2} </span>",
        pointFormatter = tooltip_chart(
          accesor = "ttdata",
          hc_opts = list(
            series = list(list(color = "#FF0000", name = "{}")),
            chart = list(type = "bar"),
            plotOptions = list(bar = list(pointWidth = 70,
                                          dataLabels = list(enabled = TRUE,
                                                            format = '{point.y}',
                                                            style = list(fontSize = '12px')))),
            xAxis = list(type = "category")),
          width = '500',
          height = '400'),
        followPointer = TRUE,
        followTouchMove = TRUE,
        shared = TRUE) %>% 
      hc_add_theme(tema) %>% 
      hc_xAxis(title = list(text = "Forma de Ingresso",style = list(fontSize = '14px',fontWeight= 'bold')),labels = list(style = list(fontSize = '14px',fontWeight= 'bold'))) %>% 
      hc_yAxis(title = list(text = "Inscritos",style = list(fontSize = '14px',fontWeight= 'bold')),labels = list(style = list(fontSize = '12px',fontWeight= 'bold'))) 
  })
}

shinyApp(ui, server)
