package org.zhang.geom

/**
 * A mesh is an undirected graph of 3d vectors.
 */
trait Mesh {
  def points:Set[Vec3]
  def edges:Set[Edge]
  def edgesFor(v:Vec3) = edges filter (_.isPoint(v))
  def pointsConnectedTo(v:Vec3) = edgesFor(v).flatMap(_.other(v))
}
class MeshImpl(val points:Set[Vec3], val edges:Set[Edge]) extends Mesh

case class TriangleMesh(triangles:Traversable[Triangle]) extends Mesh {
  lazy val points = triangles.flatMap(_.points).toSet
  lazy val edges = triangles.flatMap(_.edges).toSet
}

object Mesh {
  def apply(points:Set[Vec3], edges:Set[Edge]) = new MeshImpl(points, edges)
  def apply(tris:Traversable[Triangle]):TriangleMesh = TriangleMesh(tris)
  /**
   * <p>Returns a TriangleMesh built by taking some starting mesh and iteratively applying the surface refinement technique
   * described in <a href="http://paulbourke.net/miscellaneous/sphere_cylinder/">http://paulbourke.net/miscellaneous/sphere_cylinder/</a>.
   * @param rad radius of the approximated sphere
   * @param detail Number of times to refine the octahedron
   */
  def sphere(radius:Float, detail:Int, start:Traversable[Triangle]) = new TriangleMesh({
    //todo: optimize
    spherify(radius, Stream.iterate(start, detail+1)(_.flatMap(refine _)).last)
  })

  /**
   * <p>Creates a sphere using an octahedron as the starting shape.
   * If d is the detail, then this method returns a sphere approximation with the following features: </p> <br />
   * Number of faces: 8 * 4^d <br />
   * Number of vertices: 4 * 4^d + 2 <br />
   * Number of edges: 24 * 4^d <br />
   */
  def sphere(radius:Float, detail:Int):TriangleMesh = sphere(radius, detail, octahedron(radius).triangles)

  def tetrahedron(rad:Float = 1) = TriangleMesh({
    val angle = 2 * math.atan(math.sqrt(2)).toFloat //tetrahedral angle
    ((0 to 2).map(x => Vec3.fromSpherical(rad, TWO_PI / 3 * x, PI / 2 - angle)) :+ Vec3.fromSpherical(rad, 0, PI / 2)).
      combinations(3).map(x => Triangle(x(0), x(1), x(2))).toList
  })

  def octahedron(rad:Float = 1) = TriangleMesh({
    val centers = List(Vec3(rad, 0, 0), Vec3(0, rad, 0), Vec3(-rad, 0, 0), Vec3(0, -rad, 0))
    val centerTwos = centers.sliding(2).toList :+ Seq(centers.last, centers.head)
    List(Vec3(0, 0, rad), Vec3(0, 0, -rad)).flatMap(x => {
      centerTwos.map(s => Triangle(x, s(0), s(1)))
    })
  })

}