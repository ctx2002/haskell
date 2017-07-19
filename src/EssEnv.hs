module EssEnv
where
    data EssEnv = EssEmpty | ExtendEnv String String EssEnv

    isEmptyEssEnv :: EssEnv -> Bool
    isEmptyEssEnv EssEmpty              = True
    isEmptyEssEnv (ExtendEnv s1 s2 env) = False

    -- this is a search function.
    applyEnv :: EssEnv -> String -> String
    applyEnv EssEmpty str = error "could not find " ++ str
    applyEnv (ExtendEnv s1 s2 env) str =
        if s1 == str then s2
        else applyEnv env str

    extendEnv :: EssEnv -> String -> String -> EssEnv
    extendEnv EssEmpty key value = ExtendEnv key value EssEmpty
    extendEnv rest key value     = ExtendEnv key value rest
