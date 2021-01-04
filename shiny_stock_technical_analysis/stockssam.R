library(shiny)
library(shinydashboard)
library(quantmod)
library(tidyverse)
library(tidyquant)
library(highcharter)
library(PerformanceAnalytics)
library(rvest)
library(DT)
library(TTR)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(title = "Stock Technical Analysis",
             tabPanel("Manual",
                      sidebarLayout(
                        sidebarPanel(
                          p(strong(h6("For IDX or Indonesian stock, don't forget to put .JK in the end."))),
                          p(strong(h6("(Example: BBCA.JK)"))),
                          textInput('stock1', 'Input the stock:', "BBCA.JK"),
                          dateRangeInput('date1', 'Select date:', start = "2011-01-01")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("SMA short", h5("Buy: short-run SMA crosses from below to above a long-run SMA."), h5("Sell: short-run SMA crosses from above to below a long-run SMA."), h6("series 2: short-run SMA (10)"), h6("series 3: long-run SMA (20)"), highchartOutput('chartSMAshort1')),
                            tabPanel("SMA long", h5("Buy: short-run SMA crosses from below to above a long-run SMA."), h5("Sell: short-run SMA crosses from above to below a long-run SMA."), h6("series 2: short-run SMA (50)"), h6("series 3: long-run SMA (200)"), highchartOutput('chartSMAlong1')),
                            tabPanel("EMA short", h5("Buy: short-run EMA crosses from below to above a long-run EMA."), h5("Sell: short-run EMA crosses from above to below a long-run EMA."), h6("series 2: short-run EMA (12)"), h6("series 3: long-run EMA (26)"), highchartOutput('chartEMAshort1')),
                            tabPanel("EMA long", h5("Buy: short-run EMA crosses from below to above a long-run EMA."), h5("Sell: short-run EMA crosses from above to below a long-run EMA."), h6("series 2: short-run EMA (50)"), h6("series 3: long-run EMA (200)"), highchartOutput('chartEMAlong1')),
                            tabPanel("BBand", h5("Buy: price is above the lower band."), h5("Sell: price is below the upper band."), verbatimTextOutput("BBands1"), plotOutput('chartBBand1')),
                            tabPanel("Momentum", h5("Buy: momentum changes from negative to positive."), h5("Sell: momentum changes from positive to negative."), verbatimTextOutput("momentum1"), plotOutput('chartMomentum1')),
                            tabPanel("ROC", h5("Buy: ROC changes from negative to positive."), h5("Sell: ROC changes from positive to negative."), verbatimTextOutput("ROC1"), plotOutput('chartROC1')),
                            tabPanel("MACD", h5("Buy: MACD crosses from below to above the signal line."), h5("Sell: MACD crosses from above to below the signal line."), verbatimTextOutput("MACD1"), plotOutput('chartMACD1')),
                            tabPanel("RSI", h5("Buy: RSI < 30 (oversold/ bearish)."), h5("Sell: RSI > 70 (overbought/ bullish)."), verbatimTextOutput("RSI1"), plotOutput('chartRSI1'))
                          )
                        )
                      )
             ),
             tabPanel("IDX",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('sector2', 'Select sector:', choices = c('Agriculture', 'Mining', 'Basic Industry and Chemicals', 'Miscellaneous Industry', 'Consumer Goods Industry', 'Real Estate and Building Construction', 'Infrastructure, Utilities and Transportation', 'Finance', 'Trade, Service, and Investment')),
                          selectInput('subsector2', 'Select sub-sector:', choices = c('')),
                          selectInput('stock2', 'Select stock:', choices = c('')),
                          dateRangeInput('date2', 'Select date:', start = "2011-01-01")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("SMA short", h5("Buy: short-run SMA crosses from below to above a long-run SMA."), h5("Sell: short-run SMA crosses from above to below a long-run SMA."), h6("series 2: short-run SMA (10)"), h6("series 3: long-run SMA (20)"), highchartOutput('chartSMAshort2')),
                            tabPanel("SMA long", h5("Buy: short-run SMA crosses from below to above a long-run SMA."), h5("Sell: short-run SMA crosses from above to below a long-run SMA."), h6("series 2: short-run SMA (50)"), h6("series 3: long-run SMA (200)"), highchartOutput('chartSMAlong2')),
                            tabPanel("EMA short", h5("Buy: short-run EMA crosses from below to above a long-run EMA."), h5("Sell: short-run EMA crosses from above to below a long-run EMA."), h6("series 2: short-run EMA (12)"), h6("series 3: long-run EMA (26)"), highchartOutput('chartEMAshort2')),
                            tabPanel("EMA long", h5("Buy: short-run EMA crosses from below to above a long-run EMA."), h5("Sell: short-run EMA crosses from above to below a long-run EMA."), h6("series 2: short-run EMA (50)"), h6("series 3: long-run EMA (200)"), highchartOutput('chartEMAlong2')),
                            tabPanel("BBand", h5("Buy: price is above the lower band."), h5("Sell: price is below the upper band."), verbatimTextOutput("BBands2"), plotOutput('chartBBand2')),
                            tabPanel("Momentum", h5("Buy: momentum changes from negative to positive."), h5("Sell: momentum changes from positive to negative."), verbatimTextOutput("momentum2"), plotOutput('chartMomentum2')),
                            tabPanel("ROC", h5("Buy: ROC changes from negative to positive."), h5("Sell: ROC changes from positive to negative."), verbatimTextOutput("ROC2"), plotOutput('chartROC2')),
                            tabPanel("MACD", h5("Buy: MACD crosses from below to above the signal line."), h5("Sell: MACD crosses from above to below the signal line."), verbatimTextOutput("MACD2"), plotOutput('chartMACD2')),
                            tabPanel("RSI", h5("Buy: RSI < 30 (oversold/ bearish)."), h5("Sell: RSI > 70 (overbought/ bullish)."), verbatimTextOutput("RSI2"), plotOutput('chartRSI2'))
                          )
                        )
                      )
             )
  )
)






server <- function(input, output, session) {
  pilihan1 <- reactive({ if (input$sector2 == 'Agriculture') {
    c('Crops','Plantation','Animal Husbandary','Fishery')
  } else if (input$sector2 == 'Mining') {
    c('Coal Mining','Crude Petroleum and Natural Gas Production','Metal and Mineral Mining','Land / Stone Quarrying')
  } else if (input$sector2 == 'Basic Industry and Chemicals') {
    c('Cement','Ceramics, Glass, Procelain','Metal and Allied Products','Chemicals','Plastics and Packaging','Animal Feed','Wood Industries','Pulp and Paper','Others Basic Industry and Chemicals')
  } else if (input$sector2 == 'Miscellaneous Industry') {
    c('Machinery and Heavy Equipment','Automotive and Components','Textile, Garment','Footwear','Cable','Electronics')
  } else if (input$sector2 == 'Consumer Goods Industry') {
    c('Food and Beverages','Tobacco Manufacturers','Pharmaceuticals','Cosmetics and Household','Houseware','Others Consumer Goods Industry')
  } else if (input$sector2 == 'Property, Real Estate and Building Construction') {
    c('Property and Real Estate','Building Construction')
  } else if (input$sector2 == 'Infrastructure, Utilities and Transportation') {
    c('Energy','Toll Road, Airport, Harbor and Allied Products','Telecommunication','Transportation','Non Building Construction')
  } else if (input$sector2 == 'Finance') {
    c('Bank','Financial Institution','Securities Company','Insurance','Others Finance')
  } else {
    c('Wholesale','Retail Trade','Restaurant, Hotel and Tourism','Advertising, Printing and Media','Healthcare','Computer and Services','Investment Company','Others')
  } 
  })
  
  observe({
    updateSelectInput(session, "subsector2",
                      choices = pilihan1()
    )})
  
  pilihan2 <- reactive({ if (input$subsector2 == 'Crops') {
    c('BISI.JK')
  } else if (input$subsector2 == 'Plantation') {
    c('AALI.JK','ANDI.JK','ANJT.JK','BWPT.JK','CSRA.JK',
      'DSNG.JK','GOLL.JK','GZCO.JK','JAWA.JK','LSIP.JK',
      'MAGP.JK','MGRO.JK','PALM.JK','SGRO.JK','SIMP.JK',
      'SMAR.JK','SSMS.JK','TBLA.JK','UNSP.JK')
  } else if (input$subsector2 == 'Animal Husbandary') {
    c('BEEF.JK')
  } else if (input$subsector2 == 'Fishery') {
    c('CPRO.JK','DSFI.JK','IIKP.JK')
  } else if (input$subsector2 == 'Coal Mining') {
    c('ADRO.JK','ARII.JK','BOSS.JK','BRMS.JK','BSSR.JK',
      'BUMI.JK','BYAN.JK','DEWA.JK','DOID.JK','FIRE.JK',
      'GEMS.JK','GTBO.JK','HRUM.JK','ITMG.JK','KKGI.JK',
      'MBAP.JK','MYOH.JK','PKPK.JK','PTBA.JK','PTRO.JK',
      'SMMT.JK','TOBA.JK')
  } else if (input$subsector2 == 'Crude Petroleum and Natural Gas Production') {
    c('ARTI.JK','BIPI.JK','ELSA.JK','ENRG.JK','ESSA.JK',
      'MEDC.JK','RUIS.JK','SURE.JK','WOWS.JK')
  } else if (input$subsector2 == 'Metal and Mineral Mining') {
    c('ANTM.JK','CITA.JK','CKRA.JK','DKFT.JK','IFSH.JK',
      'INCO.JK','MDKA.JK','PSAB.JK','SMRU.JK','TINS.JK',
      'ZINC.JK')
  } else if (input$subsector2 == 'Land / Stone Quarrying') {
    c('CTTH.JK','MITI.JK')
  } else if (input$subsector2 == 'Cement') {
    c('INTP.JK','SMBR.JK','SMCB.JK','SMGR.JK','WSBP.JK',
      'WTON.JK')
  } else if (input$subsector2 == 'Ceramics, Glass, Porcelain') {
    c('AMFG.JK','ARNA.JK','CAKK.JK','IKAI.JK','KIAS.JK',
      'MARK.JK','MLIA.JK','TOTO.JK')
  } else if (input$subsector2 == 'Metal and Allied Products') {
    c('ALKA.JK','ALMI.JK','BAJA.JK','BTON.JK','CTBN.JK',
      'GDST.JK','GGRP.JK','INAI.JK','ISSP.JK','JKSW.JK',
      'KRAS.JK','LION.JK','LMSH.JK','NIKL.JK','PICO.JK',
      'PURE.JK','TBMS.JK')
  } else if (input$subsector2 == 'Chemicals') {
    c('AGII.JK','BRPT.JK','BUDI.JK','DPNS.JK','EKAD.JK',
      'ETWA.JK','INCI.JK','MDKI.JK','MOLI.JK','SRSN.JK',
      'TPIA.JK','UNIC.JK')
  } else if (input$subsector2 == 'Plastics and Packaging') {
    c('AKKU.JK','AKPI.JK','APLI.JK','BRNA.JK','ESIP.JK',
      'FPNI.JK','IGAR.JK','IMPC.JK','IPOL.JK','PBID.JK',
      'SIMA.JK','SMKL.JK','TALF.JK','TRST.JK','YPAS.JK')
  } else if (input$subsector2 == 'Animal Feed') {
    c('CPIN.JK','JPFA.JK','MAIN.JK','SIPD.JK')
  } else if (input$subsector2 == 'Wood Industries') {
    c('IFII.JK','SINI.JK','SULI.JK','TIRT.JK')
  } else if (input$subsector2 == 'Pulp and Paper') {
    c('ALDO.JK','FASW.JK','INKP.JK','INRU.JK','KBRI.JK',
      'KDSI.JK','SPMA.JK','SWAT.JK','TKIM.JK')
  } else if (input$subsector2 == 'Others Basic Industry and Chemicals') {
    c('INOV.JK','KMTR.JK')
  } else if (input$subsector2 == 'Machinery and Heavy Equipment') {
    c('AMIN.JK','ARKA.JK','GMFI.JK','KPAL.JK','KRAH.JK')
  } else if (input$subsector2 == 'Automotive and Components') {
    c('ASII.JK','AUTO.JK','BOLT.JK','BRAM.JK','GDYR.JK',
      'GJTL.JK','IMAS.JK','INDS.JK','LPIN.JK','MASA.JK',
      'NIPS.JK','PRAS.JK','SMSM.JK')
  } else if (input$subsector2 == 'Textile, Garment') {
    c('ADMG.JK','ARGO.JK','BELL.JK','CNTB.JK','CNTX.JK',
      'ERTX.JK','ESTI.JK','HDTX.JK','INDR.JK','MYTX.JK',
      'PBRX.JK','POLU.JK','POLY.JK','RICY.JK','SRIL.JK',
      'SSTM.JK','STAR.JK','TFCO.JK','TRIS.JK','UCIT.JK',
      'UNIT.JK','ZONE.JK')
  } else if (input$subsector2 == 'Footwear') {
    c('BATA.JK','BIMA.JK')
  } else if (input$subsector2 == 'Cable') {
    c('CCSI.JK','IKBI.JK','JECC.JK','KBLI.JK','KBLM.JK',
      'SCCO.JK','VOKS.JK')
  } else if (input$subsector2 == 'Electronics') {
    c('JSKY.JK','PTSN.JK','SLIS.JK')
  } else if (input$subsector2 == 'Food and Beverages') {
    c('AISA.JK','ALTO.JK','CAMP.JK','CEKA.JK','CLEO.JK',
      'COCO.JK','DLTA.JK','DMND.JK','FOOD.JK','GOOD.JK',
      'HOKI.JK','ICBP.JK','IKAN.JK','INDF.JK','KEJU.JK',
      'MLBI.JK','MYOR.JK','PANI.JK','PCAR.JK','PSDN.JK',
      'PSGO.JK','ROTI.JK','SKBM.JK','SKLT.JK','STTP.JK',
      'ULTJ.JK')
  } else if (input$subsector2 == 'Tobacco Manufacturers') {
    c('GGRM.JK','HMSP.JK','ITIC.JK','RMBA.JK','WIIM.JK')
  } else if (input$subsector2 == 'Pharmaceuticals') {
    c('DVLA.JK','INAF.JK','KAEF.JK','KLBF.JK','MERK.JK',
      'PEHA.JK','PYFA.JK','SCPI.JK','SIDO.JK','TSPC.JK')
  } else if (input$subsector2 == 'Cosmetics and Household') {
    c('ADES.JK','KINO.JK','KPAS.JK','MBTO.JK','MRAT.JK',
      'TCID.JK','UNVR.JK')
  } else if (input$subsector2 == 'Houseware') {
    c('CINT.JK','KICI.JK','LMPI.JK','WOOD.JK')
  } else if (input$subsector2 == 'Others Consumer Goods Industry') {
    c('HRTA.JK')
  } else if (input$subsector2 == 'Property and Real Estate') {
    c('ARMY.JK','APLN.JK','ASRI.JK','BAPA.JK','BAPI.JK',
      'BCIP.JK','BEST.JK','BIKA.JK','BIPP.JK','BKDP.JK',
      'BKSL.JK','BSDE.JK','CITY.JK','COWL.JK','CPRI.JK',
      'CTRA.JK','DART.JK','DILD.JK','DMAS.JK','DUTI.JK',
      'ELTY.JK','EMDE.JK','FMII.JK','FORZ.JK','GAMA.JK',
      'GMTD.JK','GPRA.JK','GWSA.JK','INDO.JK','JRPT.JK',
      'KIJA.JK','KOTA.JK','LAND.JK','LCGP.JK','LPCK.JK',
      'LPKR.JK','MDLN.JK','MKPI.JK','MMLP.JK','MPRO.JK',
      'MTLA.JK','MYRX.JK','MYRXP.JK','NIRO.JK','NZIA.JK','OMRE.JK',
      'PAMG.JK','PLIN.JK','POLI.JK','POLL.JK','POSA.JK',
      'PPRO.JK','PUDP.JK','PWON.JK','REAL.JK','RISE.JK',
      'RBMS.JK','RDTX.JK','RODA.JK','SATU.JK','SCBD.JK',
      'SMDM.JK','SMRA.JK','TARA.JK','TRIN.JK','URBN.JK')
  } else if (input$subsector2 == 'Building Construction') {
    c('ACST.JK','ADHI.JK','CSIS.JK','DGIK.JK','IDPR.JK',
      'MTRA.JK','NRCA.JK','PBSA.JK','PSSI.JK','PTPP.JK',
      'SKRN.JK','SSIA.JK','TAMA.JK','TOPS.JK','TOTL.JK',
      'WEGE.JK','WIKA.JK','WSKT.JK')
  } else if (input$subsector2 == 'Energy') {
    c('KEEN.JK','KOPI.JK','LAPD.JK','MPOW.JK','PGAS.JK',
      'POWR.JK','RAJA.JK','TGRA.JK') 
  } else if (input$subsector2 == 'Toll Road, Airport, Harbor and Allied Products') {
    c('CMNP.JK','IPCC.JK','JSMR.JK','META.JK','TEBE.JK')
  } else if (input$subsector2 == 'Telecommunication') {
    c('BTEL.JK','EXCL.JK','FREN.JK','ISAT.JK','JAST.JK',
      'TLKM.JK')
  } else if (input$subsector2 == 'Transportation') {
    c('APOL.JK','ASSA.JK','BBRM.JK','BIRD.JK','BLTA.JK',
      'BPTR.JK','BULL.JK','CANI.JK','CASS.JK','CMPP.JK',
      'DEAL.JK','GIAA.JK','HELI.JK','HITS.JK','IATA.JK',
      'INDX.JK','IPCM.JK','JAYA.JK','KARW.JK','KJEN.JK',
      'LEAD.JK','LRNA.JK','MBSS.JK','MIRA.JK','NELY.JK',
      'PURA.JK','PORT.JK','PTIS.JK','RIGS.JK','SAFE.JK',
      'SAPX.JK','SDMU.JK','SHIP.JK','SMDR.JK','SOCI.JK',
      'TAMU.JK','TAXI.JK','TCPI.JK','TMAS.JK','TNCA.JK',
      'TPMA.JK','TRAM.JK','TRUK.JK','WEHA.JK','WINS.JK',
      'ZBRA.JK')
  } else if (input$subsector2 == 'Non Building Construction') {
    c('BALI.JK','BUKK.JK','IBST.JK','GHON.JK','GOLD.JK',
      'INDY.JK','LCKM.JK','MTPS.JK','OASA.JK','PPRE.JK',
      'PTPW.JK','SUPR.JK','TBIG.JK','TOWR.JK')
  } else if (input$subsector2 == 'Bank') {
    c('AGRO.JK','AGRS.JK','AMAR.JK','ARTO.JK','BABP.JK',
      'BACA.JK','BBCA.JK','BBHI.JK','BBKP.JK','BBMD.JK',
      'BBNI.JK','BBRI.JK','BBTN.JK','BBYB.JK','BCIC.JK',
      'BDMN.JK','BEKS.JK','BGTB.JK','BINA.JK','BJBR.JK',
      'BJTM.JK','BKSW.JK','BMAS.JK','BMRI.JK','BNBA.JK',
      'BNGA.JK','BNII.JK','BNLI.JK','BSIM.JK','BSWD.JK',
      'BTPN.JK','BTPS.JK','BVIC.JK','DNAR.JK','INPC.JK',
      'MAYA.JK','MCOR.JK','MEGA.JK','NISP.JK','NOBU.JK',
      'PNBN.JK','PNBS.JK','SDRA.JK')
  } else if (input$subsector2 == 'Financial Institution') {
    c('ADMF.JK','BBLD.JK','BFIN.JK','BPFI.JK','CFIN.JK',
      'DEFI.JK','FINN.JK','FUJI.JK','HDFA.JK','IBFN.JK',
      'IMJS.JK','INCF.JK','MFIN.JK','MGNA.JK','POLA.JK',
      'TIFA.JK','TRUS.JK','VRNA.JK','WOMF.JK')
  } else if (input$subsector2 == 'Securities Company') {
    c('AKSI.JK','AMOR.JK','APIC.JK','ARTA.JK','KREN.JK',
      'OCAP.JK','PADI.JK','PANS.JK','PEGE.JK','RELI.JK',
      'TRIM.JK','YULE.JK')
  } else if (input$subsector2 == 'Insurance') {
    c('ABDA.JK','AHAP.JK','AMAG.JK','ASBI.JK','ASDM.JK',
      'ASJT.JK','ASMI.JK','ASRM.JK','JMAS.JK','LIFE.JK',
      'LPGI.JK','MREI.JK','MTWI.JK','PNIN.JK','TUGU.JK',
      'VINS.JK')
  } else if (input$subsector2 == 'Others Finance') {
    c('BCAP.JK','BPII.JK','CASA.JK','GSMF.JK','LPPS.JK',
      'MTFN.JK','PNLF.JK','SMMA.JK','VICO.JK')
  } else if (input$subsector2 == 'Wholesale') {
    c('AGAR.JK','AIMS.JK','AKRA.JK','APII.JK','AYLS.JK',
      'BLUE.JK','BMSR.JK','BOGA.JK','CARS.JK','CLPI.JK',
      'CNKO.JK','DPUM.JK','DSSA.JK','DWGL.JK','EPMT.JK',
      'FISH.JK','GREN.JK','HADE.JK','HDIT.JK','HEXA.JK',
      'HKMU.JK','INPS.JK','INTA.JK','INTD.JK','KAYU.JK',
      'IRRA.JK','JKON.JK','KOBX.JK','KONI.JK','LTLS.JK',
      'MDRN.JK','MICE.JK','MPMX.JK','OKAS.JK','OPMS.JK',
      'PMJS.JK','SDPC.JK','SPTO.JK','SQMI.JK','TFAS.JK',
      'TGKA.JK','TIRA.JK','TRIL.JK','TURI.JK','UNTR.JK',
      'WAPO.JK','WICO.JK')
  } else if (input$subsector2 == 'Retail Trade') {
    c('ACES.JK','AMRT.JK','CENT.JK','CSAP.JK','DAYA.JK',
      'DIVA.JK','ECII.JK','ERAA.JK','GLOB.JK','HERO.JK',
      'KIOS.JK','KOIN.JK','LPPF.JK','MAPA.JK','MAPI.JK',
      'MCAS.JK','MIDI.JK','MKNT.JK','MPPA.JK','NFCX.JK',
      'RALS.JK','RANC.JK','RIMO.JK','SKYB.JK','SONA.JK',
      'TELE.JK','TRIO.JK')
  } else if (input$subsector2 == 'Restaurant, Hotel and Tourism') {
    c('BAYU.JK','BUVA.JK','CLAY.JK','DFAM.JK','DUCK.JK',
      'EAST.JK','FAST.JK','FITT.JK','HOME.JK','HOTL.JK',
      'HRME.JK','ICON.JK','INPP.JK','JGLE.JK','JIHD.JK',
      'JSPT.JK','KPIG.JK','MABA.JK','MAMI.JK','MAMIP.JK','MAPB.JK',
      'MINA.JK','NASA.JK','NATO.JK','NUSA.JK','PANR.JK',
      'PDES.JK','PGJO.JK','PGLI.JK','PJAA.JK','PNSE.JK',
      'PSKT.JK','PTSP.JK','PZZA.JK','SHID.JK','SOTS.JK')
  } else if (input$subsector2 == 'Advertising, Printing and Media') {
    c('ABBA.JK','BLTZ.JK','DMMX.JK','EMTK.JK','FILM.JK',
      'FORU.JK','IPTV.JK','JTPE.JK','KBLV.JK','LINK.JK',
      'LPLI.JK','MARI.JK','MDIA.JK','MNCN.JK','MSIN.JK',
      'MSKY.JK','SCMA.JK','TMPO.JK','VIVA.JK')
  } else if (input$subsector2 == 'Healthcare') {
    c('HEAL.JK','MIKA.JK','PRDA.JK','PRIM.JK','SAME.JK',
      'SILO.JK','SRAJ.JK')
  } else if (input$subsector2 == 'Computer and Services') {
    c('ASGR.JK','ATIC.JK','DIGI.JK','DNET.JK','ENVY.JK',
      'GLVA.JK','LMAS.JK','LUCK.JK','MLPT.JK','MTDL.JK')
  } else if (input$subsector2 == 'Investment Company') {
    c('ABMM.JK','BHIT.JK','BMTR.JK','BNBR.JK','MLPL.JK',
      'NICK.JK','PLAS.JK','POOL.JK','SRTG.JK')
  } else {
    c('APEX.JK','BOLA.JK','DYAN.JK','GEMA.JK','ITMA.JK',
      'MFMI.JK','SFAN.JK','SOSS.JK','SUGI.JK','YELO.JK')
  } 
  })
  
  observe({
    updateSelectInput(session, "stock2",
                      choices = pilihan2()
    )})
  
  # Fetch Stock Price
  fetch1 <- reactive({
    getSymbols(input$stock1, 
               auto.assign = FALSE, 
               from = input$date1[1],
               to = input$date1[2])
  })
  fetch2 <- reactive({
    getSymbols(input$stock2, 
               auto.assign = FALSE, 
               from = input$date2[1],
               to = input$date2[2])
  })
  
  # Create an indicator
  SMA1_10 <- reactive({
    SMA(na.omit(Ad(fetch1())), n = 10)
  })
  SMA1_20 <- reactive({
    SMA(na.omit(Ad(fetch1())), n = 20)
  })
  SMA1_50 <- reactive({
    SMA(na.omit(Ad(fetch1())), n = 50)
  })
  SMA1_200 <- reactive({
    SMA(na.omit(Ad(fetch1())), n = 200)
  })
  EMA1_12 <- reactive({
    EMA(na.omit(Ad(fetch1())), n = 12)
  })
  EMA1_26 <- reactive({
    EMA(na.omit(Ad(fetch1())), n = 26)
  })
  EMA1_50 <- reactive({
    EMA(na.omit(Ad(fetch1())), n = 50)
  })
  EMA1_200 <- reactive({
    EMA(na.omit(Ad(fetch1())), n = 200)
  })
  
  # Render plot
  output$chartSMAshort1 <- renderHighchart({
    highchart(type = "stock") %>%
      hc_add_series(fetch1()) %>% 
      hc_add_series(SMA1_10()) %>%
      hc_add_series(SMA1_20())
  })
  output$chartSMAlong1 <- renderHighchart({
    highchart(type = "stock") %>%
      hc_add_series(fetch1()) %>% 
      hc_add_series(SMA1_50()) %>%
      hc_add_series(SMA1_200())
  })
  output$chartEMAshort1 <- renderHighchart({
    highchart(type = "stock") %>%
      hc_add_series(fetch1()) %>% 
      hc_add_series(EMA1_12()) %>%
      hc_add_series(EMA1_26())
  })
  output$chartEMAlong1 <- renderHighchart({
    highchart(type = "stock") %>%
      hc_add_series(fetch1()) %>% 
      hc_add_series(EMA1_50()) %>%
      hc_add_series(EMA1_200())
  })
  output$chartBBand1 <- renderPlot({
    chartSeries(fetch1(),
                theme=chartTheme('white'))
    addBBands(n=20,sd=2)
  })
  output$chartMomentum1 <- renderPlot({
    chartSeries(fetch1(),
                theme=chartTheme('white'))
    addMomentum(n=14)
  })
  output$chartROC1 <- renderPlot({
    chartSeries(fetch1(),
                theme=chartTheme('white'))
    addROC(n=14)
  })
  output$chartMACD1 <- renderPlot({
    chartSeries(fetch1(),
                theme=chartTheme('white'))
    addMACD(fast=12,slow=26,signal=9)
  })
  output$chartRSI1 <- renderPlot({
    chartSeries(fetch1(),
                theme=chartTheme('white'))
    addRSI(n=14)
  })
  # Create an indicator
  SMA2_10 <- reactive({
    SMA(na.omit(Ad(fetch2())), n = 10)
  })
  SMA2_20 <- reactive({
    SMA(na.omit(Ad(fetch2())), n = 20)
  })
  SMA2_50 <- reactive({
    SMA(na.omit(Ad(fetch2())), n = 50)
  })
  SMA2_200 <- reactive({
    SMA(na.omit(Ad(fetch2())), n = 200)
  })
  EMA2_12 <- reactive({
    EMA(na.omit(Ad(fetch2())), n = 12)
  })
  EMA2_26 <- reactive({
    EMA(na.omit(Ad(fetch2())), n = 26)
  })
  EMA2_50 <- reactive({
    EMA(na.omit(Ad(fetch2())), n = 50)
  })
  EMA2_200 <- reactive({
    EMA(na.omit(Ad(fetch2())), n = 200)
  })
  # Render plot
  output$chartSMAshort2 <- renderHighchart({
    highchart(type = "stock") %>%
      hc_add_series(fetch2()) %>% 
      hc_add_series(SMA2_10()) %>%
      hc_add_series(SMA2_20())
  })
  output$chartSMAlong2 <- renderHighchart({
    highchart(type = "stock") %>%
      hc_add_series(fetch2()) %>% 
      hc_add_series(SMA2_50()) %>%
      hc_add_series(SMA2_200())
  })
  output$chartEMAshort2 <- renderHighchart({
    highchart(type = "stock") %>%
      hc_add_series(fetch2()) %>% 
      hc_add_series(EMA2_12()) %>%
      hc_add_series(EMA2_26())
  })
  output$chartEMAlong2 <- renderHighchart({
    highchart(type = "stock") %>%
      hc_add_series(fetch2()) %>% 
      hc_add_series(EMA2_50()) %>%
      hc_add_series(EMA2_200())
  })
  output$chartBBand2 <- renderPlot({
    chartSeries(fetch2(),
                theme=chartTheme('white'))
    addBBands(n=20,sd=2)
  })
  output$chartMomentum2 <- renderPlot({
    chartSeries(fetch2(),
                theme=chartTheme('white'))
    addMomentum(n=14)
  })
  output$chartROC2 <- renderPlot({
    chartSeries(fetch2(),
                theme=chartTheme('white'))
    addROC(n=14)
  })
  output$chartMACD2 <- renderPlot({
    chartSeries(fetch2(),
                theme=chartTheme('white'))
    addMACD(fast=12,slow=26,signal=9)
  })
  output$chartRSI2 <- renderPlot({
    chartSeries(fetch2(),
                theme=chartTheme('white'))
    addRSI(n=14)
  })
  
  # Create an indicator
  BBands1 <- reactive({
    BBands(na.omit(Ad(fetch1())), n = 20, sd = 2)
  })
  BBands2 <- reactive({
    BBands(na.omit(Ad(fetch2())), n = 20, sd = 2)
  })
  momentum1 <- reactive({
    momentum(na.omit(Ad(fetch1())), n = 14)
  })
  momentum2 <- reactive({
    momentum(na.omit(Ad(fetch2())), n = 14)
  })
  ROC1 <- reactive({
    ROC(na.omit(Ad(fetch1())), n = 14)
  })
  ROC2 <- reactive({
    ROC(na.omit(Ad(fetch2())), n = 14)
  })
  MACD1 <- reactive({
    MACD(na.omit(Ad(fetch1())), fast=12,slow=26,signal=9)
  })
  MACD2 <- reactive({
    MACD(na.omit(Ad(fetch2())), fast=12,slow=26,signal=9)
  })
  RSI1 <- reactive({
    RSI(na.omit(Ad(fetch1())), n = 14)
  })
  RSI2 <- reactive({
    RSI(na.omit(Ad(fetch2())), n = 14)
  })
  # Text output
  tailBBands1 <- reactive({tail(BBands1(),n=5)})
  output$BBands1 <- renderPrint({
    tailBBands1()
  })
  tailBBands2 <- reactive({tail(BBands2(),n=5)})
  output$BBands2 <- renderPrint({
    tailBBands2()
  })
  tailmomentum1 <- reactive({tail(momentum1(),n=5)})
  output$momentum1 <- renderPrint({
    tailmomentum1()
  })
  tailmomentum2 <- reactive({tail(momentum2(),n=5)})
  output$momentum2 <- renderPrint({
    tailmomentum2()
  })
  tailROC1 <- reactive({tail(ROC1(),n=5)})
  output$ROC1 <- renderPrint({
    tailROC1()
  })
  tailROC2 <- reactive({tail(ROC2(),n=5)})
  output$ROC2 <- renderPrint({
    tailROC2()
  })
  tailMACD1 <- reactive({tail(MACD1(),n=5)})
  output$MACD1 <- renderPrint({
    tailMACD1()
  })
  tailMACD2 <- reactive({tail(MACD2(),n=5)})
  output$MACD2 <- renderPrint({
    tailMACD2()
  })
  tailRSI1 <- reactive({tail(RSI1(),n=5)})
  output$RSI1 <- renderPrint({
    tailRSI1()
  })
  tailRSI2 <- reactive({tail(RSI2(),n=5)})
  output$RSI2 <- renderPrint({
    tailRSI2()
  })
}

shinyApp(ui = ui, server = server)

