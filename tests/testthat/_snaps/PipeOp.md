# PipeOp printer

    Code
      print(PipeOpNOP$new())
    Output
      
      -- PipeOp <nop>: not trained ---------------------------------------------------
      Values: list()
      
      -- Input channels: 
         name  train predict
       <char> <char>  <char>
        input      *       *
      
      -- Output channels: 
         name  train predict
       <char> <char>  <char>
       output      *       *

---

    Code
      print(PipeOpDebugMulti$new(3, 4))
    Output
      
      -- PipeOp <debug.multi>: not trained -------------------------------------------
      Values: list()
      
      -- Input channels: 
          name  train predict
        <char> <char>  <char>
       input_1      *       *
       input_2      *       *
       input_3      *       *
      
      -- Output channels: 
           name  train predict
         <char> <char>  <char>
       output_1      *       *
       output_2      *       *
       output_3      *       *
       output_4      *       *

---

    Code
      print(PipeOpDebugMulti$new(100, 0))
    Output
      
      -- PipeOp <debug.multi>: not trained -------------------------------------------
      Values: list()
      
      -- Input channels: 
          name  train predict
        <char> <char>  <char>
       input_1      *       *
       input_2      *       *
       input_3      *       *
       input_4      *       *
       input_5      *       *
      [...] (95 rows omitted)
      
      -- Output channels: 
          name  train predict
        <char> <char>  <char>
       output_      *       *

---

    Code
      print(PipeOpBranch$new(c("odin", "dva", "tri")))
    Output
      
      -- PipeOp <branch>: not trained ------------------------------------------------
      Values: selection=odin
      
      -- Input channels: 
         name  train predict
       <char> <char>  <char>
        input      *       *
      
      -- Output channels: 
         name  train predict
       <char> <char>  <char>
         odin      *       *
          dva      *       *
          tri      *       *

---

    Code
      print(PipeOpLearner$new(mlr_learners$get("classif.debug")))
    Output
      
      -- PipeOp <classif.debug>: not trained -----------------------------------------
      Values: list()
      
      -- Input channels: 
         name       train     predict
       <char>      <char>      <char>
        input TaskClassif TaskClassif
      
      -- Output channels: 
         name  train           predict
       <char> <char>            <char>
       output   NULL PredictionClassif

