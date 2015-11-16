
ecm_mv n theta = 2 * theta^2 / ( (n+1) * (n+2) )
ecm_m n theta = theta^2 / (3*n)

thetas = [1,10,40]
ns = [5,30,60]

cuadrito fn ns ts = mapM_ (\n -> putStrLn $ show $ map (fn n) ts) ns

main = do
    putStrLn "Cuadrito ECM_mv"
    cuadrito ecm_mv ns thetas
    putStrLn ""
    putStrLn "Cuadrito ECM_m"
    cuadrito ecm_m ns thetas

