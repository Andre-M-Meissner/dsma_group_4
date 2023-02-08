
	SELECT t_ru.business_id6 AS business_id
			,(CASE WHEN t_ch.date3 IS NULL THEN 0 ELSE 1 END) AS ch_in
			,t_ru.date6 AS date_tip
			,t_ru.cum_max_us_fans AS cum_max_us_fans
		FROM (
			-- table3: for checkins
			SELECT t3_1.business_id AS business_id3
				,date1::DATE AS date3
			FROM (
				SELECT checkintable.business_id AS business_id
					,unnest(string_to_array(checkintable.date, ',')) AS date1
				FROM public3.checkintable, public3.businesstable
				WHERE checkintable.business_id = businesstable.business_id AND businesstable.city LIKE 'Las%Vegas' AND businesstable.state LIKE 'NV' AND businesstable.categories LIKE '%Restaurants%'
				) AS t3_1
			GROUP BY business_id3
				,date3
			) AS t_ch
		RIGHT JOIN (
			-- table6.2: a much more elegant, but more complex query
			SELECT tip_user.business_id51 AS business_id6
				,tip_user.date5 AS date6
				,tip_user.n_tips AS n_tips
				,(
					SELECT max(max_us_fans) AS cum_max_us_fans
					FROM (
						SELECT business_id
							,date AS date
							,max(users1.u_fans) AS max_us_fans
						FROM public3.tipstable
						LEFT JOIN (
							SELECT user_id
								,fans AS u_fans
							FROM public3.userstable
							) AS users1 ON tipstable.user_id = users1.user_id
						GROUP BY tipstable.business_id
							,tipstable.date
						) AS t53
					WHERE t53.business_id = tip_user.business_id51
						AND t53.DATE::DATE < tip_user.date5
					)
				
			FROM (
				SELECT t52.business_id51 AS business_id51
					,t52.date5 AS date5
					,t52.n_tips AS n_tips
				FROM (
					SELECT business_id53 AS business_id51, date53 AS date5, n_tips
					FROM (SELECT tipstable.business_id AS business_id53, date_trunc('day', generate_series
							( '2017-01-01'::timestamp 
							, '2017-01-08'::timestamp
							, '1 day'::interval))::date AS date53
						FROM public3.tipstable, public3.businesstable AS bus
						WHERE tipstable.business_id=bus.business_id AND bus.city LIKE 'Las%Vegas' AND bus.state LIKE 'NV' AND bus.categories LIKE '%Restaurants%'
						GROUP BY tipstable.business_id) AS t53
						LEFT JOIN 
							(SELECT tipstable.business_id AS business_id5x
													,(tipstable.date)::DATE AS date5x
													,COUNT(tipstable.text) AS n_tips
												FROM public3.tipstable, public3.businesstable AS bus
												WHERE tipstable.business_id=bus.business_id AND bus.city LIKE 'Las%Vegas' AND bus.state LIKE 'NV' AND bus.categories LIKE '%Restaurants%'
												GROUP BY tipstable.business_id
													,date5x) AS t5x
							ON business_id5x=business_id53 AND date5x=date53
					) AS t52
				) AS tip_user
			) AS t_ru ON t_ch.date3 = t_ru.date6
			AND t_ch.business_id3 = t_ru.business_id6;