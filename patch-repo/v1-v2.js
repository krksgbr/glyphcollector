module.exports.run = function(repo) {
  const newRepo = {
    ...repo,
    version: "2.0",
    tag: "RepoModel",
    projectRepo: repo.projectRepo.map(project => ({
      ...project,
      tag: "Project",
      glyphCollections: project.glyphCollections.map(gc => ({
        ...gc,
        tag: "GlyphCollection",
        averages: gc.averages.map(avg => ({
          ...avg,
          tag: "Avg",
          image: {
            ...avg.image,
            tag: "Image"
          }
        })),
        matches: gc.matches.map(match => ({
          ...match,
          tag: "MatchedGlyph",
          image: {
            ...match.image,
            tag: "Image"
          },
          sourceImage: {
            ...match.sourceImage,
            tag: "Image"
          },
          templateImage: {
            ...match.templateImage,
            tag: "Image"
          }
        }))
      })),
      id: {
        contents: project.id,
        tag: "ProjectId"
      },
      templates: project.templates.map(t => ({
        ...t,
        tag: "Image"
      })),
      sources: project.sources.map(s => ({
        ...s,
        tag: "Image"
      }))
    }))
  };
  return newRepo;
};
