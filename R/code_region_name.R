#' code district name in daegu.
#'
#' dg_district_code function will code district name of Daege.
#'
#' @param data data set.
#' @param new_var variable created.
#' @param region_code_var variable of region code.
#' @param dong_name_var variable name in Korean of small region.
#' @param county_code region code of Daegu in population data.
#' @return data including a new district varible.
#' @examples
#' @export

## daegu district coding
dg_district_code <- function(data, new_var="district", region_code_var="d_num", dong_name_var ="d", county_code="'27") {
  data[['district']] <- ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("동인동", '동인1_2_4가동','동인3가동', "삼덕동","성내1동","성내2동","성내3동","대신동", "남산1동","남산2동", "남산3동", "남산4동", "대봉1동", "대봉2동"), "중구",
                               ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("공산동","불로_봉무동","도평동"),"불로공산권",
                                      ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("안심1동", "안심2동", "안심3_4동"),"안심권",
                                             ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("지저동", "동촌동", "방촌동",  "해안동"), "동촌권",
                                                    ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("신암1동", "신암2동", "신암3동","신암4동", "신암5동", "신천1_2동", "신천3동", "신천4동",  "효목1동", "효목2동"), "동대구권",
                                                           ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("내당1동", "내당2_3동", "내당4동"), "내당권",
                                                                  ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("비산1동","비산2_3동","비산4동","비산5동","비산6동","비산7동", "원대동"), "비산권",
                                                                         ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("평리1동","평리2동",  "평리3동", "평리4동","평리5동", "평리6동", "상중이동"), "평리권",
                                                                                ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("이천동", "봉덕1동", "봉덕2동", "봉덕3동"),"이천봉덕권",
                                                                                       ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("대명1동","대명2동", "대명3동", "대명4동", "대명5동", "대명10동"),"대명권",
                                                                                              ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("대명6동", "대명9동", "대명11동"),"앞산권",
                                                                                                     ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("고성동", "칠성동", "침산1동","침산2동",  "침산3동","산격1동", "산격2동", "산격3동", "산격4동","대현1동","대현2동" ,"대현동", "복현1동", "복현2동", "검단동", "무태조야동", "노원동"),"강남지역",
                                                                                                            ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("관문동", "태전1동", "태전2동", "구암동", "관음동", "읍내동", "동천동", "국우동"), "강북지역",
                                                                                                                   ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("범어1동","범어2동","범어3동","범어4동","만촌1동","만촌2동", "만촌3동"), "범어만촌권",
                                                                                                                          ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("수성1가동", "수성2_3가동", "수성4가동", "황금1동", "황금2동", "중동", "상동", "파동", "두산동"), "중동권",
                                                                                                                                 ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("지산1동","지산2동","범물1동", "범물2동"), "지산범물권",
                                                                                                                                        ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("고산1동","고산2동","고산3동"),"고산권",
                                                                                                                                               ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("성당1동", "성당2동","성당동", "두류1_2동", "두류1동", "두류2동","두류3동", "감삼동", "죽전동", "용산1동", "본리동", "본동"),"두류권",
                                                                                                                                                      ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("용산2동","이곡1동","이곡2동","신당동","장기동"), "성서권",
                                                                                                                                                             ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("월성1동","월성2동","진천동", "상인1동","상인2동", "상인3동", "도원동", "송현1동", "송현2동"), "상인권",
                                                                                                                                                                    ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("다사읍","다사읍서재출장소", "하빈면"), "북부권",
                                                                                                                                                                           ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("화원읍", "옥포면", "옥포읍", "가창면"), "중부권",
                                                                                                                                                                                  ifelse(substr(data[[region_code_var]],1,3)== county_code & data[[dong_name_var]] %in% c("논공읍","논공읍공단출장소","현풍면", "현풍읍", "유가면", "유가읍", "구지면"),"남부권", NA)))))))))))))))))))))))

  return(data)
}
