# MarkAbstract
Process to put the mark &lt;h2> html tag into each 'abstract' element present in the xml files.

## Algorithm:

1. For each input xml file
2. For each '&lt;ab&gt;' , '&lt;ab_[lang]&gt;' field
3. If the field contains at least one word of the 'allowed words - type 1' followed by a collon as the patterns:
  * &lt;begin of the sentence&gt;&lt;allowed words type 1&gt;&lt;collon&gt;&lt;other words&gt; - ex: *Conclusion **:** the study...*
  * &lt;dot&gt;&lt;allowed words&gt;&lt;collon&gt;<other words type 1&gt; - ex: *...for that **.** Conclusion **:** the study...*
4. Or if the field contains one word of the 'allowed words - type 2' followed by a dot as the patterns:
  * &lt;begin of the sentence&gt;&lt;allowed words type 2&gt;&lt;dot&gt;<other words&gt; - ex: *Conclusion **.** the study...*
  * &lt;dot&gt;&lt;allowed words&gt;&lt;dor&gt;<other words type 2&gt; - ex: *...for that **.** Conclusion **.** the study...*
5. Then create a copy of the abstract field with name &lt;mark&gt; or &lt;marl_lang&gt; and mark the &lt;allowed words&gt; with &lt;h2&gt;&lt;allowed words&gt;&lt;/h2&gt;

## Allowed words - type 1

  * aim
  * aims
  * analysis
  * background
  * comentario
  * comentarios
  * comment
  * comments
  * conclusao
  * conclusion
  * conclusion(s)
  * conclusions
  * conclusiones
  * conclusoes
  * contexto
  * desenho
  * design
  * discusion
  * discussion
  * diseno
  * finding
  * findings
  * justificativa
  * justificacion
  * hypothesis
  * introducao
  * introduccion
  * introduction
  * limitation
  * limitations
  * material
  * measure
  * measures
  * method
  * methodology
  * methods
  * metodo
  * metodologia
  * metodos
  * objective
  * objectives
  * objectivo
  * objectivos
  * objetivo
  * objetivos
  * outcome
  * outcomes
  * purpose
  * purposes
  * purpose(s)
  * resultado
  * resultados
  * result
  * results
  * significance
  * subjects
  * summary
  * synthesis
  
  ## Allowed words - type 2
  
  * conclusion
  * conclusiones
  * conclusions
  * conclusao
  * conclusoes
  * method
  * methods
  * metodology
  * metodo
  * metodos
  * metodologia
  * objective
  * objectives
  * objetivo
  * objetivos
  * objectivos
  * result
  * results
  * resultado
  * resultados
