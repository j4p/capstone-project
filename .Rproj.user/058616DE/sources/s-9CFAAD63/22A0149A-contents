library(shiny)
library(tidyverse)
library(plotly)
library(readxl)
library(zoo)

header <- div(
  div(class="title",
      h1("Kesenjangan Upah dan Harga Rumah AS Makin Melebar, Indonesia Juga?")
  ),
  div(class="author",
      img(src="croppedphoto.png",
          class="author-img",
          width="50em", height="50em"),
      div(class="author-text",
          p("Juang Angger Pamungkas"),
          p("24 Maret 2022")
      )
  ),
  hr()
)

lg6Plotly <- function(title, output_id, source) {
  div(class="col-lg-6",
    h4(title),
    plotlyOutput(output_id),
    span(class="source", source)
  )
}

lg6PlotlyCenter <- function(title, output_id, source) {
  div(class="col-lg-6 col-lg-offset-3",
    h4(title),
    plotlyOutput(output_id),
    span(class="source", source)
  )
}

ui <- bootstrapPage(
  tags$head(tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "style.css")),
  tags$head(tags$title("Kesenjangan Upah dan Harga Rumah AS Makin Melebar, Indonesia Juga? | oleh Juang Angger Pamungkas")),
  div(class="container",
    header,
    div(class="content",
      HTML(
        "<p>Berdasarkan <a href=\"https://www.cnbc.com/2021/11/10/home-prices-are-now-rising-much-faster-than-incomes-studies-show.html\">berita</a>
        yang dimuat di laman CNBC, dilaporkan bahwa median harga rumah di Amerika Serikat
        tumbuh lebih jauh dibandingkan median pendapatan warga AS.
        Hal ini menyebabkan masyarakat AS pada umumnya kesulitan dalam memiliki tempat tinggal sendiri.
        </p>"),
      p("Keadaan tersebut menimbulkan pertanyaan apakah hal yang sama terjadi di Indonesia atau tidak.
        Data-data yang disajikan di bawah ini akan membantu dalam menjawab pertanyaan tersebut,"),
      div(class="row row-plot",
        lg6Plotly(title="Perubahan Median Harga Rumah vs. Perubahan Median Pendapatan (AS)",
                  output_id="us_hvi",
                  source=span("Sumber: ",
                              a(href="https://www.realestatewitch.com/house-price-to-income-ratio-2021/", "realestatewitch.com")
                             )
                  ),
        lg6Plotly(title="Perubahan Harga Rumah Rata-Rata vs. Perubahan UMR Rata-Rata (Indonesia)",
                  output_id="id_hvw",
                  source=span("Sumber: ",
                              a(href="https://www.bi.go.id/id/publikasi/laporan/Pages/SHPR-Triwulan-III-2021.aspx", "SHPR Q3 2021 (BI)"),
                              " & ",
                              a(href="https://www.bps.go.id/indicator/19/220/1/upah-minimum-regional-propinsi.html", "UMR 1997-2020 (BPS)")
                             )
                  )
      ),
      p("Berdasarkan data di atas, terlihat bahwa laju kenaikan Upah Minimum Regional di Indonesia
        masih mampu mengungguli laju kenaikan harga rumah."),
      p("Lalu bagaimana bila dibandingkan dengan pengeluaran untuk sehari-hari?"),
      p("Untuk menjawab pertanyaan tersebut kita dapat menggunakan data Indeks Harga Konsumen (IHK)
        yang pada dasarnya merupakan indikator biaya hidup masyarakat Indonesia."),
      div(class="row row-plot",
        lg6PlotlyCenter(title="Perubahan IHK vs. Perubahan UMR Rata-Rata",
                        output_id="id_cvw",
                        source=span("Sumber: ",
                                    a(href="https://www.bps.go.id/statictable/2010/01/08/1441/ihk-dan-rata-rata-upah-per-bulan-buruh-industri-di-bawah-mandor-supervisor-1996-2014-1996-100-.html", "IHK 1996-2014 (BPS)"),
                                    ", ",
                                    a(href="https://www.bps.go.id/statictable/2009/06/15/907/indeks-harga-konsumen-dan-inflasi-bulanan-indonesia-2006-2021.html", "IHK 2006-2021 (BPS)"),
                                    ", ",
                                    a(href="https://www.bps.go.id/indicator/19/220/1/upah-minimum-regional-propinsi.html", "UMR 1997-2020 (BPS)")
                                   )
                        )
      ),
      p("Berdasarkan data di atas, terlihat bahwa laju kenaikan UMR rata-rata di Indonesia
        juga masih mengungguli laju kenaikan IHK."),
      h3("Penutup"),
      p("Beberapa hal yang dapat disimpulkan dari analisis ini adalah:"),
      tags$ol(
        tags$li("Kenaikan UMR rata-rata Indonesia mampu mengungguli kenaikan harga rumah."),
        tags$li("Kenaikan UMR rata-rata Indonesia mampu mengungguli kenaikan biaya hidup.")
      ),
      p("Pemerintah dapat dikatakan telah bekerja cukup baik dalam menentukan UMR dalam kaitannya dengan harga rumah dan biaya hidup.
        Namun, pemerintah diharapkan untuk tidak lengah agar masalah yang sama tidak terjadi di Indonesia."),
      h3("Catatan"),
      p("Beberapa hal yang perlu digarisbawahi oleh pembaca mengenai analisis ini, antara lain:"),
      tags$ol(
        tags$li("Dikarenakan keterbatasan data, data yang digunakan merupakan data UMR rata-rata Indonesia dan bukan upah yang benar-benar diterima masyarakat Indonesia."),
        tags$li("Sangat memungkinkan bila perubahan rata-rata ataupun median upah yang diterima masyarakat Indonesia sifatnya cenderung lebih stagnan dibandingkan perubahan UMR rata-rata Indonesia."),
        tags$li("Penggunaan data upah yang benar-benar diterima oleh masyarakat Indonesia sangat mungkin dapat mengubah hasil dari analisis ini.")
      ),
      HTML("<p>Penulis sangat mengharapkan masukan dari pembaca. Anda dapat mengirimkan masukan Anda ke <a href=\"mailto:juangangger@gmail.com\">email saya</a>.</p>"),
      hr()
    )
  )
)

server <- function(input, output, session) {
  theme_set(theme_minimal())
  
  # Plot 1
  us_hvi_data <- read_csv("us_house_vs_income.csv") 
  names(us_hvi_data) <- make.names(names(us_hvi_data))
  names(us_hvi_data)[1] <- "Tahun"

  us_hvi_plot <- us_hvi_data %>%
    pivot_longer(c(Home.Value.Change, Income.Change), names_to="Keterangan", values_to="Perubahan (%)") %>%
    mutate(Keterangan = replace(Keterangan, Keterangan=="Home.Value.Change", "Median Harga Rumah")) %>%
    mutate(Keterangan = replace(Keterangan, Keterangan=="Income.Change", "Median Pendapatan")) %>%
    ggplot(aes(x=Tahun, y=`Perubahan (%)`, color=Keterangan, fill=Keterangan)) +
    geom_area(alpha=0.2, position="dodge") +
    scale_x_continuous(breaks = round(seq(min(us_hvi_data$Tahun), max(us_hvi_data$Tahun), by=5),1)) +
    theme(axis.text.x = element_text(angle=45, hjust=1))

  us_hvi_plot <- ggplotly(us_hvi_plot)
  output$us_hvi <- renderPlotly(us_hvi_plot)
  
  # Plot 2
  id_hvw_data <- read_xlsx("id_house_vs_minwage.xlsx")
  
  id_hvw_plot <- id_hvw_data %>%
    select(Tahun, `Perubahan Harga Rumah Rata-Rata`, `Perubahan UMR Rata-Rata`) %>%
    head(-2) %>%
    mutate(`Perubahan Harga Rumah Rata-Rata`=na.approx(`Perubahan Harga Rumah Rata-Rata`)) %>%
    mutate(`Perubahan UMR Rata-Rata`=na.approx(`Perubahan UMR Rata-Rata`)) %>%
    pivot_longer(c(`Perubahan Harga Rumah Rata-Rata`, `Perubahan UMR Rata-Rata`), names_to="Keterangan", values_to="Perubahan (%)") %>%
    mutate(Keterangan = replace(Keterangan, Keterangan=="Perubahan Harga Rumah Rata-Rata", "Harga Rumah Rata-Rata")) %>%
    mutate(Keterangan = replace(Keterangan, Keterangan=="Perubahan UMR Rata-Rata", "UMR Rata-Rata")) %>%
    ggplot(aes(x=Tahun, y=`Perubahan (%)`, color=Keterangan, fill=Keterangan)) +
    geom_area(alpha=0.2, position="dodge") +
    scale_x_continuous(breaks = round(seq(min(id_hvw_data$Tahun), max(id_hvw_data$Tahun), by=2),1)) +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  id_hvw_plot <- ggplotly(id_hvw_plot)
  output$id_hvw <- renderPlotly(id_hvw_plot)
  
  # Plot 3
  id_cvw_data <- read_xlsx("id_cpi_vs_minwage.xlsx")
  
  id_cvw_plot <- id_cvw_data %>%
    select(Tahun, `Perubahan IHK`, `Perubahan UMR Rata-Rata`) %>%
    mutate(`Perubahan UMR Rata-Rata`=na.approx(`Perubahan UMR Rata-Rata`)) %>%
    pivot_longer(c(`Perubahan IHK`, `Perubahan UMR Rata-Rata`), names_to="Keterangan", values_to="Perubahan (%)") %>%
    mutate(Keterangan = replace(Keterangan, Keterangan=="Perubahan IHK", "IHK")) %>%
    mutate(Keterangan = replace(Keterangan, Keterangan=="Perubahan UMR Rata-Rata", "UMR Rata-Rata")) %>%
    ggplot(aes(x=Tahun, y=`Perubahan (%)`, color=Keterangan, fill=Keterangan)) +
    geom_area(alpha=0.2, position="dodge") +
    scale_x_continuous(breaks = round(seq(min(id_cvw_data$Tahun), max(id_cvw_data$Tahun), by=2),1)) +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  id_cvw_plot <- ggplotly(id_cvw_plot)
  output$id_cvw <- renderPlotly(id_cvw_plot)
}

shinyApp(ui, server)
