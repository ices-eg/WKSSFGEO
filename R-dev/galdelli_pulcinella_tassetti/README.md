


Overview
================
This R code aims to analyze Automatic Identification System (AIS) data, 
and is an improvement of the method described in Galdelli et al. ([2019](#ref-Galdelli2018))

<p>

The processing workflow was developed on historical annotated data of the
 Adriatic Sea and aims to (i) identify individual fishing trips and (ii) 
 classify them on a monthly basis according to 5 predefined gear classes:
 bottom otter trawl (OTB), pelagic pair trawl (PTM), beam trawl (TBB), 
 purse seine (PS), and "other" fishing (OTHER, including dredges and 
 passive gears).

<p>

In this repository we release also:

  - a small subset of AIS signals broadcasted by vessels contained in the validated dataset (.csv);
  - all the parameters required to process the data, such as the input parameters needed to 
  classify fishing trips (.csv) and the trained Random Forest model (.rds) used to finally 
  assign the gear on a monthly basis and finalized to work in the Adriatic Sea
  - additional spatial layers required by the data processing (.shp).

<p>

A detailed description of the methodology is provided in the [Release notes](https://github.com/MAPSirbim/AIS_data_processing/tree/main/documents). <br>



# References

<div id="refs" class="references">

<div id="ref-Galdelli2019">

Galdelli, A., Mancini, A., Tassetti, A. N., Ferrà Vega, C., Armelloni, E., Scarcella, G., Fabi, G., et al. 2019. 
"A Cloud Computing Architecture to Map Trawling Activities Using Positioning Data." 
In Volume 9: 15th IEEE/ASME International Conference on Mechatronic and Embedded Systems and Applications. 
American Society of Mechanical Engineers. <https://asmedigitalcollection.asme.org/IDETC-CIE/proceedings/IDETC-CIE2019/59292/Anaheim,>
California, USA/1070321. DOI: 10.1115/DETC2019-97779

</div>

</div>






