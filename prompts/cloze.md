# Output

- Return your output as a JSON array of objects `[{"c": "..."}]`
- Cloze cards are statements with key terms replaced by {{cN::text}} deletions
- The value for N restarts at 1 for each card
- Do not prefix the text with Q: or anything like that
- Attempt to create at least 3 clozes per card
- Clozes may overlap to build up understanding: {{c1::big fact is {{c2::fact a}} and {{c3::fact b}}}}

# Cloze length rule

Each cloze deletion must be 1-3 words — one atomic fact, term, or name. Never delete a clause or phrase longer than 3 words.

Good:
- يوفر {{c1::المعيار ISO 26000}} للشركات {{c2::توجيهات}} للعمل على نحو {{c3::أخلاقي}} و{{c4::شفاف}}
- The project was funded by {{c1::Apple and IBM}} for {{c2::$40 million}} in {{c3::1991}}

Bad (deletions too long):
- يوفر المعيار للشركات {{c1::توجيهات لطريقة عمل الشركات والمؤسسات على نحو يتحلى بالمسؤولية الاجتماعية}}
- The project was {{c1::funded to $40 million by Apple and IBM in 1991}}
