# SHP 파일로 한국 위경도 정보 얻기
 - 전국 행정동_법정동 연계 자료(https://drive.google.com/drive/u/1/folders/1044OoFYFJ9_pgFI9nPTA-HiBI3mIyCee)

## 전국 법정동 데이터
 - [전국 법정동 SHP zip 파일](http://www.gisdeveloper.co.kr/?p=2332)

## 전국 행정동 데이터
 - [전국 행정동 geojson 파일](https://github.com/vuski/admdongkor)
 - [전국 행정동 SHP zip 파일(단 pjr파일 누락)](https://sgis.kostat.go.kr/contents/shortcut/shortcut_05.jsp)

## Tableau에서 지도 파일을 사용하는 방법
 - [사용방법 참고](https://www.youtube.com/watch?v=DHxR-oyk_2o)

 [geojson 파일 변환 사이트](https://www.statsilk.com/maps/convert-esri-shapefile-map-geojson-format )
 - prj, dbf, shp, shx 모두 반환

 [geojson to shpfile](https://mapshaper.org/) 지도 파일 변환 웹 프로그램

 1. mapshaper로 geojson 파일을 드래그 앤 드랍한다. 
 2. 우측 상단의 simplify로 지도를 단순화 할 수 있다.
 3. Export 메뉴로 지도 형태를 반환할 수 있다. (태블로에서 사용하려면 shpfile형태로 다운로드 받는다)
 4. [Tableau-Shapefile-to-Polygon-Converter](https://gallery.alteryx.com/#!app/Tableau-Shapefile-to-Polygon-Converter/5296f89120aaf905b8e7fb48)에서 RUN을 실행한다.
 5. 다운받은 shp zip 파일을 업로드 한다.
 6. 변환이 끝나면 RUN을 실행하고 csv 파일로 저장한다.



