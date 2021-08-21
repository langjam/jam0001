import { unified } from 'unified';
import remarkParse from 'remark-parse';
import remarkGfm from 'remark-gfm';
const src = `
# Things I'm here to do
[$TodoList]::
- [ ] kick ass
- [ ] chew bubble gum

[Heading]::
# Things I have
- [ ] bubble gum

\`\`\`bubbglegum
def check(n) {
    test
}
\`\`\`

[$]: ayy (
    [self Print:"lmao"]
)

[$TodoList]: kickass (
  [self CheckItem:"kick ass"]
  if false or |true and true| or true {
    a
    !@#$%^&*
    {}:"<>?|\`~\\,./
  }
)
`;
// [TodoList kickass]
// [self ayy]
const tree = unified()
    .use(remarkParse)
    .use(remarkGfm)
    .parse(src);
console.log(tree);
