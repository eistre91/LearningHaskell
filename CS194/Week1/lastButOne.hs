lastButOne xs = last (init xs)

lastButOne' a = if null (tail (tail a))
				then head a
				else lastButOne (tail a)