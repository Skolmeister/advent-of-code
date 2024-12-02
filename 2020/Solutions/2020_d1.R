library(magrittr)



input <- c(1749L, 1897L, 881L, 1736L, 1161L, 1720L, 1676L, 305L, 264L, 1904L, 1880L, 1173L, 483L, 1978L, 1428L, 1635L, 1386L, 1858L, 1602L, 1916L, 1906L, 1212L, 1730L, 1777L, 1698L, 1845L, 1812L, 1922L, 1729L, 1803L, 1761L, 1901L, 1748L, 1188L, 1964L, 1935L, 1919L, 1810L, 1567L, 1849L, 1417L, 1452L, 54L, 1722L, 1784L, 1261L, 1744L, 1594L, 1526L, 1771L, 1762L, 1894L, 1717L, 1716L, 51L, 1955L, 1143L, 1741L, 1999L, 1775L, 1944L, 1983L, 1962L, 1198L, 1553L, 1835L, 1867L, 1662L, 1461L, 1811L, 1764L, 1726L, 1927L, 1179L, 1468L, 1948L, 1813L, 1213L, 1905L, 1371L, 1751L, 1215L, 1392L, 1798L, 1823L, 1815L, 1923L, 1942L, 1987L, 1887L, 1838L, 1395L, 2007L, 1479L, 1752L, 1945L, 1621L, 1538L, 1937L, 565L, 1969L, 1493L, 1291L, 1438L, 1578L, 1770L, 2005L, 1703L, 1712L, 1943L, 2003L, 1499L, 1903L, 1760L, 1950L, 1990L, 1185L, 1809L, 1337L, 1358L, 1743L, 1707L, 1671L, 1788L, 1785L, 1972L, 1863L, 1690L, 1512L, 1963L, 1825L, 1460L, 1828L, 1902L, 1874L, 1755L, 1951L, 1830L, 1767L, 1787L, 1373L, 1709L, 1514L, 1807L, 1791L, 1724L, 1859L, 1590L, 1976L, 1572L, 1947L, 1913L, 1995L, 1728L, 1624L, 1731L, 1706L, 1782L, 1994L, 1851L, 1843L, 1773L, 1982L, 1685L, 2001L, 1346L, 1200L, 1746L, 1520L, 972L, 1834L, 1909L, 2008L, 1733L, 1960L, 1280L, 1879L, 1203L, 1979L, 1133L, 1647L, 1282L, 1684L, 860L, 1444L, 1780L, 1989L, 1795L, 1819L, 1797L, 1842L, 1796L, 1457L, 1839L, 1853L, 1711L, 1883L, 1146L, 1734L, 1389L)

# Part 1 ------------------------------------------------------------------
#Do it complicated via a summary value matrix with tidyverse
summary_matrix <- function(input_no){
  
  output_vector <- purrr::map_int(1:length(input), ~input[input_no]+input[.]) %>% 
    setNames(input_no)
  
  if(input_no == 1){
  
  output <<- tibble::tibble(output_vector) %>% 
    setNames(input[input_no])
  } else {
    
    output <<- output %>% 
      dplyr::bind_cols(.,tibble::tibble(output_vector) %>% setNames(input[input_no]))
    
  }
}

purrr::map_df(1:length(input), summary_matrix)

output <- output %>% 
  tibble::add_column(input = input, .before = 1) 

result_vector <- output %>% 
  dplyr::filter_all(dplyr::any_vars(. == 2020)) %>% 
  dplyr::pull(input) %>% 
  prod()



# Part 2 ------------------------------------------------------------------

#Do it fast with brute force bullshit  
repeat{
  sample <- sample(input, 3)
  x <- sum(sample)
  if(x == 2020){
    result <<- prod(sample)
    break
  }
}


