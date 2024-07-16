package util

import java.time.ZonedDateTime
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

object DateTimeUtil {

  private final val ISODateTimeFormatter: DateTimeFormatter =
    new DateTimeFormatterBuilder().appendInstant(3).toFormatter
  
  def zonedNow(): String = {
    ISODateTimeFormatter.format(ZonedDateTime.now());
  }
  def addMonthsToDate(month: Int): String = {
    ISODateTimeFormatter.format(ZonedDateTime.now().plusMonths(month));
  }

  def addWeeksToDate(week: Int): String = {
    ISODateTimeFormatter.format(ZonedDateTime.now().plusWeeks(week));
  }

  def addDaysToDate(day: Int): String = {
    ISODateTimeFormatter.format(ZonedDateTime.now().plusDays(day));
  }

  def addMonthsToDate(date: ZonedDateTime, month: Int): String = {
    ISODateTimeFormatter.format(date.plusMonths(month));
  }

  def subtractMonthsFromDate(month: Int): String = {
    ISODateTimeFormatter.format(ZonedDateTime.now().minusMonths(month))
  }

}
