Config { 
    --font = "xft:Droid Sans Mono:size=9:bold:antialias=true"
    font = "xft:bitstream vera sans mono:size=9:antialias=true",
    bgColor = "#000000",
    fgColor = "#ffffff",
    position = Top,
    lowerOnStart = False,
    commands = 
        [ Run Memory ["-t","<used>/<total>(<cache>)","-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10        
        , Run Date "%d.%m %H:%M:%S" "date" 10
        , Run MultiCpu [ "--template" , "<autototal>"
            , "--Low"      , "50"         -- units: %
            , "--High"     , "85"         -- units: %
            , "--low"      , "gray"
            , "--normal"   , "darkorange"
            , "--high"     , "darkred"
            , "-c"         , " "
            , "-w"         , "3"
        ] 50
        , Run Battery ["-t","<left>%/<timeleft>","-L","50","-H","75","-h","green","-n","yell","-l","red"] 10
        , Run CoreTemp [ "--template" , "<core0> <core1>"
            , "--Low"      , "70"        -- units: °C
            , "--High"     , "80"        -- units: °C
            , "--low"      , "darkgreen"
            , "--normal"   , "darkorange"
            , "--high"     , "darkred"
        ] 50
        , Run StdinReader
        , Run Kbd [("ru", "Ru"), ("us", "Us")] ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{[%kbd%][%coretemp%][%multicpu%][%memory%][%battery%][<fc=#FFFFCC>%date%</fc>]"
}
