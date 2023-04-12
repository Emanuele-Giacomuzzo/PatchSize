load(here("data", "morphology", "t0.RData"))
morph_mvt$video_replicate = NA
morph_mvt$video_replicate[morph_mvt$file == "sample_00001"] = 1
morph_mvt$video_replicate[morph_mvt$file == "sample_00002"] = 2
morph_mvt$video_replicate[morph_mvt$file == "sample_00003"] = 3
morph_mvt$video_replicate[morph_mvt$file == "sample_00004"] = 4
morph_mvt$video_replicate[morph_mvt$file == "sample_00005"] = 5
morph_mvt$video_replicate[morph_mvt$file == "sample_00006"] = 6
morph_mvt$video_replicate[morph_mvt$file == "sample_00007"] = 7
morph_mvt$video_replicate[morph_mvt$file == "sample_00008"] = 8
morph_mvt$video_replicate[morph_mvt$file == "sample_00009"] = 9
morph_mvt$video_replicate[morph_mvt$file == "sample_00010"] = 10
morph_mvt$video_replicate[morph_mvt$file == "sample_00011"] = 11
morph_mvt$video_replicate[morph_mvt$file == "sample_00012"] = 12
save(morph_mvt,
     file = here("data", "morphology", "t0.RData"))

load(here("data", "morphology", "t1.RData"))
morph_mvt$video_replicate = 1
save(morph_mvt,
     file = here("data", "morphology", "t1.RData"))

load(here("data", "morphology", "t2.RData"))
morph_mvt$video_replicate = 1
save(morph_mvt,
     file = here("data", "morphology", "t2.RData"))

load(here("data", "morphology", "t3.RData"))
morph_mvt$video_replicate = 1
save(morph_mvt,
     file = here("data", "morphology", "t3.RData"))

load(here("data", "morphology", "t4.RData"))
morph_mvt$video_replicate = 1
save(morph_mvt,
     file = here("data", "morphology", "t4.RData"))

load(here("data", "morphology", "t5.RData"))
morph_mvt$video_replicate = 1
save(morph_mvt,
     file = here("data", "morphology", "t5.RData"))

load(here("data", "morphology", "t6.RData"))
for(sample in 1:110){
  morph_mvt$video_replicate[morph_mvt$file == paste0("sample_0000",sample)] = 1
}
for(sample in 111:220){
  morph_mvt$video_replicate[morph_mvt$file == paste0("sample_0000",sample)] = 2
}
save(morph_mvt,
     file = here("data", "morphology", "t6.RData"))

load(here("data", "morphology", "t7.RData"))
for(sample in 1:110){
  morph_mvt$video_replicate[morph_mvt$file == paste0("sample_0000",sample)] = 1
}
for(sample in 111:220){
  morph_mvt$video_replicate[morph_mvt$file == paste0("sample_0000",sample)] = 2
}
save(morph_mvt,
     file = here("data", "morphology", "t7.RData"))
