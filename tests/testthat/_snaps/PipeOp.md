# PipeOp printer

    Code
      print(PipeOpNOP$new())
    Message
      
      -- PipeOp nop: not trained -----------------------------------------------------
      values: list()
      
      -- Input channels: 
    Output
         name  train predict
       <char> <char>  <char>
        input      *       *
    Message
      
      -- Output channels: 
    Output
         name  train predict
       <char> <char>  <char>
       output      *       *

---

    Code
      print(PipeOpDebugMulti$new(3, 4))
    Message
      
      -- PipeOp debug.multi: not trained ---------------------------------------------
      values: list()
      
      -- Input channels: 
    Output
          name  train predict
        <char> <char>  <char>
       input_1      *       *
       input_2      *       *
       input_3      *       *
    Message
      
      -- Output channels: 
    Output
           name  train predict
         <char> <char>  <char>
       output_1      *       *
       output_2      *       *
       output_3      *       *
       output_4      *       *

---

    Code
      print(PipeOpDebugMulti$new(100, 0))
    Message
      
      -- PipeOp debug.multi: not trained ---------------------------------------------
      values: list()
      
      -- Input channels: 
    Output
          name  train predict
        <char> <char>  <char>
       input_1      *       *
       input_2      *       *
       input_3      *       *
       input_4      *       *
       input_5      *       *
      [...] (95 rows omitted)
    Message
      
      -- Output channels: 
    Output
          name  train predict
        <char> <char>  <char>
       output_      *       *

---

    Code
      print(PipeOpBranch$new(c("odin", "dva", "tri")))
    Message
      
      -- PipeOp branch: not trained --------------------------------------------------
      values: selection=odin
      
      -- Input channels: 
    Output
         name  train predict
       <char> <char>  <char>
        input      *       *
    Message
      
      -- Output channels: 
    Output
         name  train predict
       <char> <char>  <char>
         odin      *       *
          dva      *       *
          tri      *       *

---

    Code
      print(PipeOpLearner$new(mlr_learners$get("classif.debug")))
    Message
      
      -- PipeOp classif.debug: not trained -------------------------------------------
      values: list()
      
      -- Input channels: 
    Output
         name       train     predict
       <char>      <char>      <char>
        input TaskClassif TaskClassif
    Message
      
      -- Output channels: 
    Output
         name  train           predict
       <char> <char>            <char>
       output   NULL PredictionClassif

