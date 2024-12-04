//// A module containing functions for converting lambda ast into a mermaid
//// flowchart diagram

import gleam/erlang/charlist
import gleam/int
import gleam/result
import lambda_calc/ast
import simplifile

pub opaque type MermaidFlowchart {
  MermaidFlowchart(String)
}

/// Creates a mermaid link between two nodes.
/// The resulting string will look like this:
///
/// - `N<count_a> --- |<node relation>| N<count_b>\n`
///
/// node_count_a: Node number of first node
/// node_count_b: Node number of connected node
/// node_relation: What node_b is to node_a
fn create_link(
  from node_count_a: Int,
  to node_count_b: Int,
  with node_relation: String,
) -> String {
  "N"
  <> int.to_string(node_count_a)
  <> " --- |"
  <> node_relation
  <> "| "
  <> "N"
  <> int.to_string(node_count_b)
  <> "\n"
}

@external(erlang, "os", "cmd")
fn run_cmd(command: charlist.Charlist) -> String

pub fn flowchart_to_image(
  fl: MermaidFlowchart,
  file_name: String,
) -> Result(Nil, Nil) {
  let MermaidFlowchart(fl) = fl

  use _ <- result.try(
    simplifile.write(contents: fl, to: file_name) |> result.nil_error,
  )

  let cmd =
    {
      "mmdc --height 1080 --width 1920 -i "
      <> file_name
      <> " -o "
      <> file_name
      <> ".png"
    }
    |> charlist.from_string

  run_cmd(cmd)

  Ok(Nil)
}

/// Turns the ast into a mermaid flowchart string
/// so it can be viewed in the browser via [mermaid.live](https://mermaid.live)
pub fn to_mermaid_flowchart(ast_node: ast.ASTNode) -> MermaidFlowchart {
  let doc = "flowchart TD\n"
  let #(node, _) = to_mermaid_flowchart_impl(ast_node, 0)
  MermaidFlowchart(doc <> node)
}

/// TODO: Make code prettier, i guess
fn to_mermaid_flowchart_impl(
  ast_node: ast.ASTNode,
  counter: Int,
) -> #(String, Int) {
  case ast_node {
    ast.AssignmentNode(assignment) -> {
      let current_counter = counter
      let tree = "N" <> int.to_string(current_counter) <> "[Assignment]\n"

      let bound_var = assignment.variable
      let expression = assignment.expression
      let in = assignment.in

      let bound_var_counter = counter + 1
      let #(node, counter) =
        to_mermaid_flowchart_impl(
          ast.VariableNode(bound_var),
          bound_var_counter,
        )
      let tree = tree <> node

      let expression_counter = counter + 1
      let #(node, counter) =
        to_mermaid_flowchart_impl(expression, expression_counter)
      let tree = tree <> node

      let in_counter = counter + 1
      let #(node, counter) = to_mermaid_flowchart_impl(in, in_counter)
      let tree = tree <> node

      let tree =
        tree
        <> create_link(
          from: current_counter,
          to: bound_var_counter,
          with: "Identifier",
        )

      let tree =
        tree
        <> create_link(
          from: current_counter,
          to: expression_counter,
          with: "Bound Expression",
        )

      let tree =
        tree <> create_link(from: current_counter, to: in_counter, with: "In")

      #(tree, counter)
    }

    ast.ApplicationNode(application) -> {
      let current_counter = counter
      let tree = "N" <> int.to_string(current_counter) <> "[Application]\n"

      let left_side = application.left_side
      let right_side = application.right_side

      let left_side_counter = counter + 1
      let #(node, counter) =
        to_mermaid_flowchart_impl(left_side, left_side_counter)
      let tree = tree <> node

      let right_side_counter = counter + 1
      let #(node, counter) =
        to_mermaid_flowchart_impl(right_side, right_side_counter)
      let tree = tree <> node

      let tree =
        tree
        <> create_link(
          from: current_counter,
          to: left_side_counter,
          with: "Abstraction",
        )

      let tree =
        tree
        <> create_link(
          from: current_counter,
          to: right_side_counter,
          with: "Value",
        )

      #(tree, counter)
    }

    ast.AbstractionNode(abstraction) -> {
      let current_counter = counter
      let tree = "N" <> int.to_string(current_counter) <> "(Abstraction)\n"

      let ident = ast.VariableNode(abstraction.bound_ident)
      let in = abstraction.body

      let body_counter = counter + 1
      let #(node, counter) = to_mermaid_flowchart_impl(in, body_counter)
      let tree = tree <> node

      let bind_counter = counter + 1
      let #(node, counter) = to_mermaid_flowchart_impl(ident, bind_counter)
      let tree = tree <> node

      let tree =
        tree
        <> create_link(from: current_counter, to: body_counter, with: "Body")

      let tree =
        tree
        <> create_link(from: current_counter, to: bind_counter, with: "Bind")

      #(tree, counter)
    }

    ast.VariableNode(var) -> {
      let tree =
        "N" <> int.to_string(counter) <> "([Variable: " <> var.name <> "])\n"

      #(tree, counter + 1)
    }

    ast.ConstantNode(constant) -> {
      let tree =
        "N"
        <> int.to_string(counter)
        <> "([Variable: "
        <> constant.value |> int.to_string
        <> "])\n"

      #(tree, counter + 1)
    }
  }
}
