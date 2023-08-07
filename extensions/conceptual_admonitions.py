from docutils import nodes
from docutils.parsers.rst import Directive

from sphinx.locale import _
from sphinx.util.docutils import SphinxDirective

class keyIdeaDirective(SphinxDirective):

    has_content = True

    def run(self):
        key_idea_node = keyIdea(self.content)
        key_idea_node += nodes.Title(_('Key Idea'), _('Key Idea'))
        self.state.nested_parse(self.content, self.content_offset, key_idea_node)
        return [key_idea_node]

class keyIdea(nodes.Admonition, nodes.Element):
    pass

def visit_keyidea_node(self,node):
    self.visit_admonition(node)

def depart_keyidea_node(self, node):
    self.depart_admonition(node)

def setup(app):
    app.add_node(keyIdea,
                 html=(visit_keyidea_node, depart_keyidea_node),
                 latex=(visit_keyidea_node, depart_keyidea_node),
                 text=(visit_keyidea_node, depart_keyidea_node))
    app.add_directive("keyIdea", keyIdeaDirective)

    return {
        'version': '0.1',
        'parallel_read_safe': True,
        'parallel_write_safe': True,
    }
