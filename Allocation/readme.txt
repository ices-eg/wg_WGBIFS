Station allocation Proceedure

0-Get CPUE
	Get the cpue from the previus three years in the corresponding quarter
	make two averages over the 3 year period, CPUE by areasection (cod stocks areas 22-24 and 25-28), and CPUE by depth starta. Both also by subdivision.
	
1-create planned stations list
	From the agreed upon number of planned stations pr nation and areasection, the number of hauls pr area is weiged by the CPUE files from step 0.
	Fixed number of stations by area and nation is set, e.g. Denmark always takes 5 stations in Øresund 
	The number of hauls by Sub-division and depth stratum is calculated based on the number of hauls by SD calculated above: 
	
2-Select stations from the trawl databse
	From the weighted number of station pr starta (area and depth starta) created above, is used as a wish list, and random stations in the areas are assigned to be twrawled.
	The program utelises a funtion that makes sure hauls that are very close are not selected together. it does the following:
		1) groups together hauls that are closer then 2.4 nm within a starata
		2) randomly selects one from each group of hauls
	If not enough station was found within a strata it is tried to move the missing number of stations within a strata to a new depth layer within the same area.
	If still not eneough stations within a starta it is tried to find a station in a new area (closest available area within areasection)
	If still not eough station has been selected from the trawl databse, the remaing hauls are chosen manually.
	
3-Assign country to the selected hauls
	Each planned haul selected from the trawl databse, gets assigned a country to trawl it 
	here procimity to the country location is taken into acount, as well as historical area covered.
	By square it is looked at of the square is usually surveyd by one or multiple countries; 
		If a square usually is only surveyd by one country this country will be assigned all the hauls in this square.
		If a square is shared bewteen countries, the hauls within are assigned at random to each of the countries that ussually surveys this square.
	Mnual adjustments are often needed to make the number of stations aech country has commited to, correspond to the assigned hauls.
		Directly reassign hauls to the country where the sailing path makes sense in relation to the other assigned hauls, so that the number of hauls pr country fits
	Manula country change of single hauls that are too out of the way, is done till the sailing rout makes sense, but iedeally not to the point where no overlap in area/square is pressent.
	
4-Select additional hauls
	If a country need alternative auls ore have time for additional hauls it can be selected from here, where the official plan is taken into account.
	Hauls here are also selected to be not too close to the other hauls, but can also be chosen regerless of proximity to other hsauls. 