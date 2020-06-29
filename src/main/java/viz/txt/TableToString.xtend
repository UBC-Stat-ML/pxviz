package viz.txt

import com.google.common.collect.Table
import java.util.List
import java.util.HashMap
import java.util.Map
import com.google.common.collect.HashBasedTable
import java.util.ArrayList
import java.util.Collection

class TableToString {
  
  static def void main(String [] args) {
    val Table<Integer,Integer,String> table = HashBasedTable.create()
    table => [
      put(1,1,"1,1\n1,2 continued")
      put(2,2,"asdf")
    ]
    println(table.format(borders(table.rowKeySet), borders(table.columnKeySet)))
  }
  
  /* technically return union type of T and Border */
  def static <T> List<T> borders(Collection<T> rowsOrCols) {
    val List result = new ArrayList
    result.add(new Border)
    for (rowOrCol : rowsOrCols) {
      result.add(rowOrCol)
      result.add(new Border)
    }
    return result
  }

  def static <R,C> String format(Table<R,C,?> table) {
    return format(table, table.rowKeySet.toList, table.columnKeySet.toList)
  }

  def static <R,C> String format(Table<R,C,?> table, List<R> rows, List<C> cols) {
    val builder = new StringBuilder
    val Map<R,Integer> widths = widths(table)
    for (row : rows) {
      if (row instanceof Border) {
        for (col : cols) { 
          if (col instanceof Border) {
            builder.append(col.corner)
          } else {
            builder.append((0 ..< widths.get(col)).map[row.horizontal].join(""))
          }
        }
        builder.append("\n")
      } else {
        val contents = new HashMap<C,List<String>>
        var int nSubRows = 1
        for (col : cols) 
          if (!(col instanceof Border)) {
            val split = (table.get(row, col) ?: "").toString.split("\n")
            nSubRows = Math.max(split.size, nSubRows)
            contents.put(col, split)
        }
        for (i : 0 ..< nSubRows) {
          for (col : cols) {
            if (col instanceof Border) builder.append(col.vertical)
            else {
              val currentBlock = contents.get(col)
              if (i >= currentBlock.size)
                builder.append(pad("", widths.get(col)))
              else
                builder.append(pad(currentBlock.get(i), widths.get(col)))
            }
          }
          builder.append("\n")
        }
      }
    }
    return builder.toString
  }
  
  private def static <R,C> Map<R,Integer> widths(Table<R, C, ?> table) {
    val result = new HashMap<R,Integer>()
    for (row : table.rowKeySet.filter[!(it instanceof Border)])
      for (col : table.columnKeySet.filter[!(it instanceof Border)]) {
        var cur = result.get(row) ?: 0
        cur = Math.max(cur, (table.get(row, col) ?: "").toString.split("\n").map[length].max)
        result.put(row, cur)
      }
    return result
  }
  
  def static String pad(String b, int upTo) {
    val int toAdd = upTo - b.length
    if (toAdd < 0) 
      throw new RuntimeException
    return b + (0 ..< toAdd).map[" "].join("")
  }
  
  static class Border {
    def char vertical()   { '│' }
    def char horizontal() { '─' }
    def char corner()     { '┼' }
  }
  

}