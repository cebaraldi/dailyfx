package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws._
import play.api.data._
import play.api.data.Forms._
import concurrent.ExecutionContext.Implicits.global
import com.itextpdf.text._
import com.itextpdf.text.pdf._

import java.io._
import java.io.File
import scala.swing.Dialog
import play.api.libs.Files.TemporaryFile

object Application extends Controller {

  val tmpDataPath = System.getProperty("java.io.tmpdir")
  //  var appName = ""
  //  var version = ""
  var subject = ""
  var name = ""
  var date = ""
  var rates: Array[(String, String)] = null

  def capitalz(line: String): String = {
    ((line toLowerCase) split " " map (_ capitalize)).mkString(" ")
  }

  def index = Action.async {
    //    appName = play.Play.application.configuration.getString("appName")
    //    version = play.Play.application.configuration.getString("version")
    val url = play.Play.application.configuration.getString("urlEcbDaily")
    val xml = WS.url(url).get
    xml.map(response => {
      val fx = response.xml
      subject = capitalz((fx \\ "subject").map(_.text).head)
      name = capitalz((fx \\ "name").map(_.text).head)
      date = (fx \\ "@time").toString
      rates =
        (fx \\ "@currency").map(_.text).zip(
          (fx \\ "@rate").map(_.text)).toArray.sorted
      //println(rates.getClass.getSimpleName)
      //      Ok(views.html.dailyfx(subject, name, date, rates, version, appName))
      Ok(views.html.index(subject, name, date, rates))
    })

  }

  def handle = Action { implicit request =>
    request.getQueryString("action") match {
      case Some("pdf") => pdf
      case Some("csv") => csv
      case _ => BadRequest("This action is not allowed")
    }
  }

  def cleanTmp(path: String, ext: String) {
    for {
      files <- Option(new File(path).listFiles)
      file <- files if file.getName.endsWith("." + ext)
    } file.delete()
  }

  def csv = {
    cleanTmp(tmpDataPath, "csv")
    val outfile = createFileSpecifier(tmpDataPath, date)
    val outcsv = outfile + ".csv"
    val out = new PrintWriter(outcsv)
    try {
      out.println("Currency, Rate")
      for (r <- rates) {
        out.println(r._1 + ", " + r._2)
      }
    } catch {
      case ex: FileNotFoundException =>
        Dialog.showMessage(null, "File not found", "Error", Dialog.Message.Error, null)
        ex.printStackTrace()
      case ex: IOException =>
        Dialog.showMessage(null, "There was an error while writing the file", "Error", Dialog.Message.Error, null)
        ex.printStackTrace()
      //      case _ : printStackTrace()
    } finally {
      out.close
    }
    Ok.sendFile(new java.io.File(outcsv))
  }

  def pdf = {
    cleanTmp(tmpDataPath, "pdf")
    val outfile = createFileSpecifier(tmpDataPath, date)
    val outpdf = outfile + ".pdf"
    val marginLeft = 50
    val marginRight = 50
    val marginTop = 50
    val marginBottom = 50
    val doc = new Document(PageSize.A4,
      marginLeft, marginRight, marginTop, marginBottom)

    /*
     *  Creation of PdfWriter object; other writers are HtmlWriter, RtfWriter, XmlWriter, 
     *  and several others
     */
    val fos = new FileOutputStream(outpdf)
    try {
      val pdf = PdfWriter.getInstance(doc, fos)
      pdf.setBoxSize("art", new Rectangle(36, 54, 559, 788));
      pdf.setPageEvent(new HeaderFooter)
      //      val header = new HeaderFooter(new Phrase("Add Header Part Here"), false)
      //      val footer = new HeaderFooter(new Phrase("Add Footer Here"), new Phrase("."))

      //      doc.setHeader(header)
      //      doc.setFooter(footer)

      doc.open

      // Creation of paragraph object
      doc.add(new Paragraph(name, FontFactory.getFont(
        FontFactory.HELVETICA, 20, Font.BOLD,
        new CMYKColor(0xFF, 0xFF, 0xFF, 0)))) // black
      doc.add(new Paragraph(subject + "\n" + date, FontFactory.getFont(
        FontFactory.HELVETICA, 14, Font.BOLD,
        new CMYKColor(0xFF, 0xFF, 0, 0)))) // blue

      // Creation of table object
      val table = new PdfPTable(2)
      table.setWidthPercentage(30)
      table.setSpacingBefore(25)
      table.setSpacingAfter(25)

      // Table header
      var cellCurrency = new PdfPCell
      var cellRate = new PdfPCell
      cellCurrency = new PdfPCell(new Paragraph("Currency", FontFactory.getFont(
        FontFactory.HELVETICA, 14, Font.BOLD,
        new CMYKColor(0xFF, 0xFF, 0, 0))))
      cellRate = new PdfPCell(new Paragraph("Rate", FontFactory.getFont(
        FontFactory.HELVETICA, 14, Font.BOLD,
        new CMYKColor(0xFF, 0xFF, 0, 0))))
      cellCurrency.setHorizontalAlignment(Element.ALIGN_CENTER)
      cellRate.setHorizontalAlignment(Element.ALIGN_CENTER)
      table.addCell(cellCurrency)
      table.addCell(cellRate)

      // Populate the table
      for (r <- rates) {
        cellCurrency = new PdfPCell(new Paragraph(r._1))
        cellRate = new PdfPCell(new Paragraph(r._2))
        cellCurrency.setHorizontalAlignment(Element.ALIGN_CENTER)
        cellRate.setHorizontalAlignment(Element.ALIGN_RIGHT)
        table.addCell(cellCurrency)
        table.addCell(cellRate)
      }
      doc.add(table)

      // Chunk cl = new Chunk(Image.getInstance("images/Copyleft.png"), 0, -15)
      // Paragraph p = new Paragraph("CopyLeft")
      // p.add(cl)
      // p.add("2014 ceb")
      // cb.beginText()
      // cb.moveText(36, 806)
      // cb.setFontAndSize(bf, 24)
      // cb.moveTextWithLeading(0, -36)
      // cb.showText(text)
      // cb.newlineText()
      // PdfTextArray array = new PdfTextArray("A")
      // array.add(120)
      // array.add("W")
      // array.add(120)
      // array.add("A")
      // array.add(95)
      // array.add("Y again")
      // cb.showText(array)
      // cb.endText();
    } catch {
      case ex: FileNotFoundException =>
        Dialog.showMessage(null, "File not found", "Error", Dialog.Message.Error, null)
        ex.printStackTrace()
      case ex: IOException =>
        Dialog.showMessage(null, "There was an error while writing the file", "Error", Dialog.Message.Error, null)
        ex.printStackTrace()
    } finally {
      //println(doc.bottomMargin())
      doc.close
      fos.close
    }
    Ok.sendFile(new java.io.File(outpdf))
  }

  class HeaderFooter extends PdfPageEventHelper {
    override def onEndPage(pdf: PdfWriter, doc: Document) {
      val rect = pdf.getBoxSize("art")
      (pdf.getPageNumber() % 2) match {
        case 0 =>
        //          ColumnText.showTextAligned(pdf.getDirectContent(),
        //            Element.ALIGN_RIGHT, new Phrase("even header"),
        //            rect.getRight(), rect.getTop(), 0)
        case 1 =>
        //          ColumnText.showTextAligned(pdf.getDirectContent(),
        //            Element.ALIGN_LEFT, new Phrase("odd header"),
        //            rect.getLeft(), rect.getTop(), 0)
      }
      //      ColumnText.showTextAligned(pdf.getDirectContent(),
      //        Element.ALIGN_CENTER, new Phrase("page " + pdf.getPageNumber()),
      //        (rect.getLeft() + rect.getRight()) / 2, rect.getBottom() - 18, 0);
      val appName = play.Play.application.configuration.getString("appName")
      val version = play.Play.application.configuration.getString("version")
      ColumnText.showTextAligned(pdf.getDirectContent(),
        Element.ALIGN_RIGHT, new Phrase(appName + ", version " + version + ": CopyLeft, 2014 ceb"),
        rect.getRight(), rect.getBottom() - 18, 0);
    }
  }

  def createFileSpecifier(path: String, fileName: String): String = {
    import java.util.Date
    import java.text.ParseException
    import java.text.SimpleDateFormat

    // Some date format conversions String to Date and backwards
    val inFormat = new SimpleDateFormat("yyyy-MM-dd")
    val outFormat = new SimpleDateFormat("yyyyMMdd")

    // Instantiation of document object
    path + "/fx_" + outFormat.format(inFormat.parse(date))
  }

}
