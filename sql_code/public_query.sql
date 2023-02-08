SELECT t_ruc.business_id
	,ch_in
	,date_tip
	,business_lat
	,business_long
    ,bike_park
    ,street_park
    ,lot_park
    ,touristy
    ,back_music
    ,free_wifi
    ,delivery
    ,breakfast
    ,lunch
    ,dinner
	,business_price
	,business_open
	FROM (
		-- table1: for business
		SELECT business_id AS business_id1
			,latitude AS business_lat
			,longitude AS business_long
			,(CASE
				WHEN STRPOS(((attributes)::json) ->> 'BikeParking', 'True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS bike_park
			,(CASE
				WHEN STRPOS(((attributes)::json) ->> 'BusinessParking', '''garage'': True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS garage_park
			,(CASE
				WHEN STRPOS(((attributes)::json) ->> 'BusinessParking', '''valet'': True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS valet_park
			,(CASE
				WHEN STRPOS(((attributes)::json) ->> 'BusinessParking', '''street'': True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS street_park
			,(CASE
				WHEN STRPOS(((attributes)::json) ->> 'BusinessParking', '''lot'': True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS lot_park
            ,(CASE
				WHEN STRPOS(((attributes)::json) ->> 'BusinessParking', '''validated'': True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS validated_park
			,(CASE
				WHEN STRPOS(((attributes)::json) ->> 'Ambience', '''touristy'': True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS touristy
			,(CASE
				WHEN STRPOS(((attributes)::json) ->> 'Music', '''background_music'': True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS back_music
			,(CASE
				WHEN STRPOS(((attributes)::json) ->> 'WiFi', 'free') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS free_wifi
			,(CASE
				WHEN STRPOS(((attributes)::json) ->> 'RestaurantsDelivery', 'True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS delivery
			,(CASE WHEN STRPOS(((attributes)::json) ->> 'GoodForMeal', '''breakfast'': True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS breakfast
			,(CASE WHEN STRPOS(((attributes)::json) ->> 'GoodForMeal', '''lunch'': True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS lunch
			,(CASE WHEN STRPOS(((attributes)::json) ->> 'GoodForMeal', '''dinner'': True') <> 0
					THEN 'true'
					ELSE 'false'
				END) AS dinner
			,CAST(((attributes)::json) ->> 'RestaurantsPriceRange2' AS INTEGER) AS business_price
			,is_open AS business_open
		FROM public3.businesstable
		WHERE STRPOS(categories, 'Restaurants') <> 0 AND city LIKE 'Las%Vegas' AND state LIKE 'NV'
	) AS t_bp
	,(
	SELECT t_ru.business_id6 AS business_id
			,(CASE WHEN t_ch.date3 IS NULL THEN 0 ELSE 1 END) AS ch_in
			,t_ru.date6 AS date_tip
		FROM (
			-- table3: for checkins
			SELECT t3_1.business_id AS business_id3
				,date1::DATE AS date3
			FROM (
				SELECT checkintable.business_id AS business_id
					,unnest(string_to_array(checkintable.date, ',')) AS date1
				FROM public3.checkintable, public3.businesstable
				WHERE checkintable.business_id = businesstable.business_id AND businesstable.city LIKE 'Las%Vegas' AND businesstable.state LIKE 'NV'
				) AS t3_1
			GROUP BY business_id3, date3
			) AS t_ch
		RIGHT JOIN (
			SELECT t52.business_id51 AS business_id6
				,t52.date5 AS date6
				FROM (
					SELECT business_id53 AS business_id51, date53 AS date5
					FROM (SELECT tipstable.business_id AS business_id53, date_trunc('day', generate_series
							( '2017-01-01'::timestamp 
							, '2017-12-31'::timestamp
							, '1 day'::interval))::date AS date53
						FROM public3.tipstable, public3.businesstable AS bus
						WHERE tipstable.business_id=bus.business_id AND bus.city LIKE 'Las%Vegas' AND bus.state LIKE 'NV'
						GROUP BY tipstable.business_id) AS t53
						LEFT JOIN 
							(SELECT tipstable.business_id AS business_id5x
													,(tipstable.date)::DATE AS date5x
												FROM public3.tipstable, public3.businesstable AS bus
												WHERE tipstable.business_id=bus.business_id AND bus.city LIKE 'Las%Vegas' AND bus.state LIKE 'NV'
												GROUP BY tipstable.business_id, date5x) AS t5x
							ON business_id5x=business_id53 AND date5x=date53
					) AS t52
			) AS t_ru ON t_ch.date3 = t_ru.date6 AND t_ch.business_id3 = t_ru.business_id6
	) AS t_ruc
WHERE t_bp.business_id1 = t_ruc.business_id;